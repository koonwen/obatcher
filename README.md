# obatcher
**obatcher** OCaml framework for building thread-safe "Implicitly
Batched Concurrent" data structures that are by design, performant and
easy to reason about.

# Motivation

### What is batch-processing?
Batch processing is a technique typically used for increasing data
throughput of IO-bound workloads. As the name indicates, systems that
leverage batch-processing are structured around collecting a number of
requests before processing the whole batch at one go.

This concept of "processing a batch at one go" is not to be confused
with parallel systems in which multiple threads make requests to some
shared resource which can be handled in parallel.

> A helpful analogy is to imagine that batch-processing as a singular
> waiter collating all the orders from the diners before passing it on
> to the kitchen. In contrast, multi-threaded systems are multiple
> waiters going back and forth passing orders to the kitchen but each
> time with only one order.

### Where is it used?
Typically, batch-processing is implemented in database systems that
must service many small IO requests. In such cases, the IO bandwidth
of the system ends up being under-utilized with many wasted cycles
because of the latency for each request. Waiting to collect a batch of
requests to send at one shot effectively amortizes the cost of
performing each singular request. [Nagles's
algorithm](https://en.wikipedia.org/wiki/Nagle%27s_algorithm) which is
implemented in efficient TCP/IP networks is another use case of
batching to improve overall throughput.

Batch-processing has recently found it's way into other use cases.
The new asynchronous interface in Linux - **io-uring** uses
batch-processing help applications to reduce the overhead of system
calls. Uring allows applications to queue up multiple IO requests for
the kernel to perform and post them at one go with a singulare system
call.

Furthermore, new research suggests that batch-processing is a useful
design pattern for structuring concurrent data structures. "Batch
parallel data structures (BPDS)" is a subset of thread-safe concurrent
structures. Unlike typical concurrent structures that use locks or
careful ordering to prevent data races, BPDS specify that only one
batch runs at a time. Synchronization of parallel operations are
performed upon entry to the BPDS. Requests that are made when the BPDS
is busy are sent to form a batch that runs next.

Additionally, it turns out that recieving a "batch" of a-priori known
operations brings forth more opportunities to bake parallelism into
batch handling. Furthermore various tricks of batch reordering or
batch optimizations can be used to increase throughput whilst
preserving sequential consistency.

### Why aren't "batched concurrent data structures" popular?
Potentially one of the big reasons why there has been not much uptake
is that batching in itself is extra overhead. Unlike in IO-bound
workloads, it's not obvious that collecting a set of operations will
have pay-off in the speedup of batched operations. Much more than
that, restructuring programs to explicitly batch their requests so
that they can interface with BPDS is costly too.

### Our solution
The **BATCHER** paper made some initial steps toward exemplifying a
system that could implicitly batch requests in order to preserve the
same atomic data structure interface and have batching work under the
hood. This however, required adding support directly into the
programming language's runtime scheduler.

In our library **obatcher**, we leverage OCaml's native **effects**
system through **Picos**, an interoperable effects-based concurrency
library. This allows us to cleanly separate our puzzle into 3 distinct
pieces. The batched data structure, the scheduler and support for
implicit batching. In practical application, batched data structure
designers implements their BPDS against the interface provided by
**obatcher** and can tap on the underlying Picos API's to introduce
parallelism and concurrency within their batch-processing. Separately,
the scheduler can also be implemented from scratch as long as it is
written to be Picos-compatible. Finally, **obatcher** provides an easy
to use functor to hook in implicit batching to their data structure.

Together, users have efficient BPDS that interfaces like any atomic
concurrent data structure. Along with the ability to swap in any
scheduler that is Picos compliant.

> Our library here was initially targeted at data structures but
> implicit batching goes beyond just this. As mentioned,
> batch-processing shows quick and easy wins in situations that are
> IO-bound. Our DS_sig interface is generic enough to support any
> producer-consumer interaction model whereby the underlying consumer
> can take advantage of batching.

### Results
Our approach here is new with few benchmarks. Though as an immediate
win, the ***Flat-combiner** paper which essentially studies the
cooperative batching mechanism to synchronize parallel requests to the
underlying data structure (much like what we use in
BPDS). Demonstrates that having this thin layer in front of the data
structure which then processes the requests sequentially, scales much
better than systems where threads race to hold the lock. In our own
microbenchmarking validates this as our batched skip-list shows to
have better performance than that of a coarse grained locked version
but is unable to overcome the lockfree skiplist.

## What are Batched Concurrent Data Structures (BCDS)?
BCDS are data structures that are expected to _recieve_ and
_processes_ all operations in "batches", whereby each "batch" can be
some $n > 0$ number of operations.

In practice, This means that BCDS will have a single entry point
function which takes in a "batch" which can be encoded as a List, Set,
Array, etc...

``` ocaml
type ds
type op
val run : ds -> sched_ctx -> op array -> unit
```

is the application of **Batch-parallelism** which is driven by
the idea that

> efficiently processing a batch of a priori known operations in parallel is easier than optimising performance for a stream of arbitrary asynchronous concurrent requests.

[paper]((https://dl.acm.org/doi/10.1145/2555243.2555284)** which is a
novel)
