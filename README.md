# obatcher
**obatcher** OCaml framework for building thread-safe "Batched
Concurrent" data structures that are by design, performant and easy to
reason about.

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
val run : ds -> Task.pool -> op array -> unit
```

is the application of **Batch-parallelism** which is driven by
the idea that

> efficiently processing a batch of a priori known operations in parallel is easier than optimising performance for a stream of arbitrary asynchronous concurrent requests.

[paper]((https://dl.acm.org/doi/10.1145/2555243.2555284)** which is a
novel)

implement new design approach for thread-safe data structures. The
principle behind it being that it is easier to

In the design space of Concurrent data
structures (Data structures which are thread safe)


It leverages `Promises` provided by [Eio](https://github.com/ocaml-multicore/eio)

## Why

## How
