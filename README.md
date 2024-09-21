[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https://ocaml.ci.dev/badge/koonwen/obatcher/main&logo=ocaml)](https://ocaml.ci.dev/github/koonwen/obatcher)
- [API documentation](https://koonwen.github.io/obatcher/)

# obatcher
OCaml design framework for building batch-parallel "services". Based
on ["Concurrent structures made
easy"](https://www.conference-publishing.com/download.php?Event=OOPSLAB24MAIN&Paper=23c3e926862612a7e34e440df3bba1&Version=final)
study. Whilst the paper was written with a focus on batched **data
structures**, it is discovered that the mechanism can be generalized
to work on any type of "service" which stand to benefit from
processing requests in batches. A "service" collectively refers to any
software component that exposes a request API interface.

### Contents
* [Description](#description)
* [Benefits of batch-parallel service design](#benefits-of-batch-parallel-service-design)
* [Example usage](#example-usage)
* [Batching in the wild](#batching-in-the-wild)
* [Notes](#notes)

# Description
**obatcher** is primarily a design pattern for building efficient
concurrent services.  The key observation is that _"processing a batch
of a priori known operations in parallel is easier than optimising
performance for a stream of arbitrary asynchronous concurrent
requests"._  In other words, our approach works by __separating__ (1)
the processing of requests from (2) their reception in a parallel
environment.

(1) Instead of having the hard task of optimizing concurrent services,
we think it's much easier to optimize services that take a batch of
operations as input. Additionally, an **important and neccessary
invariant** is that:

> only 1 batch may run at any time.

(2) However, designing services this way is unergonomic for users.  It
requires their programs to intentionally and efficiently collect
requests in batches (e.g. array or list) before handing them of to the
service. **obatcher** solves this by cleverly leveraging the scheduler
to automate the collection of batches in a efficient and low latency
way. From a usages perspective, interfacing with a batched service
after composing over it with **obatcher** looks like any ordinary
service that handles atomic requests.

## Benefits of batch-parallel service design
* [Batch optimization & Incremental parallelism](#batch-optimization-&-incremental-parallelism)
* [Picos scheduler swappable](#picos-scheduler-swappable)
* [Thread-safety](#thread-safety)
* [Easy to test and reason about](#easy-to-test-and-reason-about)

### Batch optimization & Incremental parallelism
The benefit of taking a batch of operations as input, is that it
potentially enables optimizations based on the spread of
operations. Furthermore, pre-analysis of operations can better advise
the parallel strategy employed by the service. Another neat advantage
of batched requests is that parallelism can be added incrementally
across operations that are "known" to be independent rather than
having to guarantee safety across all incoming concurrent operations
to the service.

### Picos scheduler swappable
**obatcher** depends on scheduler primatives to enable implicit
batching transformation on services. As a consequence, it suffers from
portability issues across different schedulers. To account for this,
**obatcher** is built on top of
[picos](https://www.github.com/polytipic/picos). Picos provides the
low-level building blocks for writing schedulers. By using the same
picos primatives to implement **obatcher**, any picos scheduler is
also compatible with **obatcher**.

### Thread-safety
A defining invariant of batched services is that only **a single batch
of operations runs at any time**. To guarantee this, **obatcher** adds
an efficient lock-free queue in front of the service to collect
operations in batches before submitting it to the service. This design
takes inspiration from
[**Flat-combining**](https://people.csail.mit.edu/shanir/publications/Flat%20Combining%20SPAA%2010.pdf). The
research shows that this synchronization method provides better
scaling properties as compared to employing naive coarse-grained
locking.

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

# Notes

### Request Latency
A frequent question about **obatcher** that comes up is:

> "Does obatcher wait for a certain number of operations to queue
> before handing it off to the service?"

The underlying question here is "How do batches form?". **obatcher**
does not wait for requests to form before launching a batch. The
design is such that any incoming request will try to launch the batch
immediately. If a batch is already in-flight, then it forms the next
incoming batch because of the single batch in-flight
invariant. Therefore, the answer is that batches form only when
another batches are being processed which means that you can expect
that your request gets serviced promptly.

### Backpressure
A convenient feature of services following the **obatcher** design pattern is
that there is a quick way to tell how efficient the underlying batch processing
mechanism that you've implemented is against your expected rate of incoming
concurrent requests.  Functionally, the ideal observation you should make with
respect to the number of request in each batch overtime is that it increases
and then plateaus at some point. This indicates that the batch processing
mechanism will eventually settle at some fixed point where it throttles at the
optimal batch size. This occurs when the throughput of request processing
matches that of the rate of incoming requests. Conceptually this works because
the more requests there are in a batch, the faster the overall throughput will
be. Therefore, the general rule of thumb is that if your batches just increase
monotonically, the batch processing implementation needs some work. If the
batches settle at some batch size, you're golden and you can work toward
bringing that number down further. Finally, if you're consistently getting a
batch size of 1, then your workload request rate might not be large enough to
have required batch processing in the first place!

### Benchmarks
Our approach here is new with unfortunately few benchmarks.  However,
**obatcher** is designed almost identically to the one described in
["Concurrent structures made
easy"](https://www.conference-publishing.com/download.php?Event=OOPSLAB24MAIN&Paper=23c3e926862612a7e34e440df3bba1&Version=final).
You can see from the results in the paper that batched structures
scales much better than those where threads race for mutual exclusion.
