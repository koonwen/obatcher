# obatcher
OCaml design framework for building batch-parallel "services". Based
on ["Concurrent structures made
easy"](https://www.arxiv.org/abs/2408.13779) study. Whilst the paper
was written with a focus on batched **data structures**, it is
discovered that the mechanism can be generalized to work on any type
of "service", where a "service" refers to any modular component of
software that we interact with via API's.

### Contents
* [Description](#description)
* [Benefits of batch-parallel service design](#benefits-of-batch-parallel-service-design)
* [Example usage](#example-usage)
* [Batching in the wild](#batching-in-the-wild)
* [Results](#results)

# Description
At it's core, **obatcher** is primarily an approach to designing
efficient concurrent services.  The key observation being that
_"processing a batch of a priori known operations in parallel is
easier than optimising performance for a stream of arbitrary
asynchronous concurrent requests"._  However, designing such services
with API's that expects users to pass in an explicit batch (e.g. array
or list) of operations is unergonomic and requires systems to be
designed around this pattern. **obatcher** solves this by cleverly
wrapping explicitly batched services and then use the scheduler to
implicitly batch operations under the hood before passing it to the
service. From the clients perspective, interfacing with the batched
service looks like any other plain atomic request service.

## Benefits of batch-parallel service design
* [Picos scheduler agnostic](#picos-scheduler-agnostic)
* [Thread-safe](#thread-safe)
* [Batch optimization & Incremental parallelism](#batch-optimization-&-incremental-parallelism)
* [Easy to test and reason about](#easy-to-test-and-reason-about)

### Picos scheduler agnostic
**obatcher** depends on scheduler primatives to perform
transformations on services. As a consequence, it suffers from
portability issues across different schedulers. To account for this,
**obatcher** is built on top of
[picos](https://www.github.com/polytipic/picos). Picos provides the
low-level building blocks for writing schedulers. By using the same
picos primatives to implement **obatcher**, any picos scheduler
becomes compatible with **obatcher**.

### Thread-safe
A defining invariant of batched services is that only **a single batch
of operations runs at any time**. To guarantee this, **obatcher** adds
an efficient lock-free queue in front of the service to collect
operations in batches before submitting it to the service. This design
takes inspiration from
[**Flat-combining**](https://people.csail.mit.edu/shanir/publications/Flat%20Combining%20SPAA%2010.pdf). The
research shows that this synchronization method provides better
scaling properties as compared to coarse-grained locking.

### Batch optimization & Incremental parallelism
The benefit of taking a batch of operations as input is that this
enables a whole slew optimizations based on the spread of
operations. Furthermore, pre-analysis of operations can better advise
how to add parallelism which can be evolved overtime rather than
having to guarantee safety across all operations to the service.

### Easy to test and reason about
Because services only handle single batches at any time, this makes it
fairly easy to design tests that are reproducable just by fixing the
same input batch.

# Example usage
Concretely, **obatcher** library consist of 2 things. A signature of
service and a functor against that signature. The service signature is
simple but crucially expects users to implement their services to
handle batches of operations. Services may then perform optimizations
based on the spread of operations in the batch and furthermore leverge
parallelism via primatives provided by the underlying picos scheduler
to speed up processing.

```ocaml
module type Service = sig
  type t
  type cfg

  type 'a op
  type wrapped_op =
    | Mk : 'a op * 'a Picos.Computation.t -> wrapped_op
        (** [wrapped_op] binds the operation on the service with it's
        corresponding suspended continuation to run after its
        completion.  *)

  val init : ?cfg:cfg -> unit -> t
  val run : t -> wrapped_op array -> unit
end
```

A simple example of a counter that abides by this signature is

```ocaml
module BatchedCounter = struct
  type t = int ref
  type cfg = unit

  let init ?cfg:_ () = ref 0

  type _ op = Incr : unit op | Decr : unit op | Get : int op
  type wrapped_op = Mk : 'a op * 'a Computation.t -> wrapped_op

  let run (t : t) (ops : wrapped_op array) =
	Array.iter (function
	| Mk (Incr, comp) -> incr t; Computation.return comp ()
	| Mk (Decr, comp) -> decr t; Computation.return comp ()
    | Mk (Get, comp) ->  Computation.return comp !t)
end
```

The issue with this counter is that users now need to form explicit
batches of requests. This can become incredibly complicated when
software grows and is also a serious balancing act between optimizing
for latency or for throughput. For that reason, we provide a functor
that composes over your batched services to make batching work
implicitly and return back to interfacing with the service with
individual operations.

```ocaml
module Make : functor (S : Service) -> sig
  type t
  val init : ?cfg:S.cfg -> unit -> t

  val exec : t -> 'a S.op -> 'a
  (** [exec t op] is the API call for a singular operation on the
      service with operations being automatically batched before
      passed to the service *)
end

include Obatcher.Make (BatchedCounter)

let incr t = exec t Incr
let decr t = exec t Decr
let get t = exec t Get
```

Now running your program in a Picos scheduler, you have a thread-safe
concurrent counter which synchronizes parallel requests and batches
them before submitting them to the counter. Our batched counter is not
very smart, it just processes requests in order like a
**Flat-combiner**. To demonstrate some types of optimizations that we
could do, we first realize that sequential consistency is preserved
even if operations are reordered. In particular, we can say that all
`Get` requests are processed at the beginning of the batch. For `Incr`
and `Decr`, these operations are commutative which means we can use a
parallel-for-reduce to gather the total change to the counter. As such
we now have:

```ocaml
  let run (t : t) (ops : wrapped_op array) =
    let len = Array.length ops in
		let start = !t in
    let delta =
      Utils.parallel_for_reduce
        ~n_fibers:(Domain.recommended_domain_count () - 1)
        ~start:0 ~finish:(len - 1)
        ~body:(fun i ->
          match ops.(i) with
          | Mk (Incr, comp) ->
              Computation.return comp ();
              1
          | Mk (Decr, comp) ->
              Computation.return comp ();
              -1
          | Mk (Get, comp) ->
              Computation.return comp start;
              0)
        ( + ) 0
    in
    t := (start + delta)

```

# Batching in the wild
Databases commonly use batching to service many small IO requests. In
such cases, the IO bandwidth of the system ends up being
under-utilized with many wasted cycles because of the latency for each
request. Waiting to collect a batch of requests to send at one shot
effectively amortizes the cost of performing each singular
request.

[Nagles's
algorithm](https://en.wikipedia.org/wiki/Nagle%27s_algorithm) is the
network equivalent of request batching. The algorithm solves the small
packet problem by batching packets before sending them out. This is
used in many efficient TCP/IP networks.

With spectre and meltdown mitigations, the cost of each syscall
context switch has increased.  The new asynchronous interface in
Linux - **io-uring** uses batch-processing help applications to reduce
the overhead of system calls. Uring allows applications to queue up
multiple IO requests for the kernel to perform and post them at one go
with a singular system call. See [examples/uring](examples/uring) for
an example of how we can use obatcher to wrap uring to get implicit
batching.

**obatcher** is based on "batch parallel data structures
(BPDS)". Unlike typical concurrent structures that use locks or
careful ordering to prevent data races, BPDS specify that only one
batch runs at a time. Synchronization of parallel operations are
performed upon entry to the BPDS. Requests that are made when the BPDS
is busy are sent to form a batch that runs next.

# Results
Our approach here is new with unfortunately few benchmarks. However,
**obatcher** is designed almost identically to the one described in
["Concurrent structures made
easy"](https://www.arxiv.org/abs/2408.13779). You can see from the
results in the paper that batched structures scales much better than
those where threads race for mutual exclusion.
