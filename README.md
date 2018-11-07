# Class 10

## Dealing with Complexity

> There are two ways of constructing a software design: one way is to
> make it so simple that there are *obviously* no deficiencies, and
> the other is to make it so complicated that there are no *obvious*
> deficiencies.

  Tony Hoare

> Computing is the only profession in which a single mind is obliged
> to span the distance from a bit to a few hundred megabytes, a ratio
> of 1 to 10^9, or nine orders of magnitude.  Compared to that number
> of semantic levels, the average mathematical theory is almost flat.
> By evoking the need for deep conceptual hierarchies, the automatic
> computer confronts us with a radically new intellectual challenge
> that has no precedent in our history.

  Edsger Dijkstra

> Software's Primary Technical Imperative has to be managing
> complexity.

  Steve McConnell


A critical aspect of software design is *problem decomposition*, which
is to minimize the amount of essential complexity that has to be dealt
with at any one point in time.  In most cases, this is the *top
priority* when designing software.

An important tool in managing complexity and achieving problem
decomposition is *information hiding*, which is to encapsulate
complexity so that it is not accessible outside of a small part of the
program. This technique has several benefits:

* Helps to decomposes large software systems into small reuusable components

* Safeguards integrity of data

* Helps to compartmentalize run-time errors

* Reduces risk of name conflicts


## Modules

A *module* is a programming language construct that enables problem
decomposition, information hiding, and (often) separate compilation.

A module

* defines a set of logically related entities (*strong internal coupling*)

* has a *public interface* that defines entities exported by the component

* may include other (private) entities that are not exported
  (*information hiding*)

* may depend on the entities defined in the interface of other
  components (*weak external coupling*)

Conceptually, a module is somewhat like a record, but with an
important distinction: 

* a record consists of a set of names called *fields*, which refer to
  values in the record

* a module consists of a set of names, which can refer to values,
  types, subroutines, other language-specific entities, and possibly other
  modules.

Different languages use different terms for this concept and the
semantics of the corresponding language constructs can differ between
languages (sometimes significantly). Examples: 

* Ada: packages

* C: header files

* C++: header files, classes, namespaces

* Java: interfaces, classes, and packages

* Scala: traits, classes, (package) objects, and packages

* OCaml: modules and functors

Important question in the design of a module system for a programming
language include:

* How are the public interface and private implementation specified?

* How are dependencies between modules resolved?

* How do the interfaces of similar models relate? Is there a notion of
  module types?

* What are the naming conventions of imported entities?

* What is the relationship between modules and source code files?

* How does a module control whether a client can access its contents?

* Must names be explicitly imported from outside modules (*closed
  modules*) or are they accessible by default (*open modules*).

We will explore two points in this design space over the next three
weeks (OCaml modules, today), and Scala's classes and objects (next
week and the week after Thanksgiving).

## OCaml's Module System


### Module Signatures

The public interface of a module is called a *signature* or *module
type*. It consists of a sequence of declarations of module members,
Module members can be types (including exception types), values, and
nested modules.

As an example, suppose we want to provide a module that implements
FIFO queues. Such queues support the following operations:

* create an empty queue

* check whether a given queue is empty

* enqueue an element into a queue, yielding the a queue with the added
  element

* dequeue an element from the queue, yielding the dequeued element and
  the new queue with that element removed.

```ocaml
module type QueueType =
  sig
    type 'a queue
    val empty : 'a queue
    val is_empty : 'a queue -> bool
    val enqueue : 'a -> 'a queue -> 'a queue
    val dequeue : 'a queue -> 'a * 'a queue
    exception Empty
  end
```

Note that the signature does not specify what that type is (i.e. how
it is implemented). It only specifies that this type exists and that
it is parametric in some other type `'a`.



### Module Structures

```ocaml
module Queue : QueueType =
  struct
    type 'a queue = 'a list
    exception Empty
        
    let empty = []

    let is_empty q =
      q = empty

    let enqueue x q =
      List.append q [x]
        
    let dequeue = function
      | [] -> raise Empty
      | x :: q -> x, q
  end
```

```ocaml
module Queue : QueueType =
  struct
    type 'a queue = 'a list * 'a list
    exception Empty
    
    let empty =
      [], []

    let is_empty q =
      empty = q

    let enqueue x (q, r) =
      q, x :: r
    
    let rec dequeue = function
      | [], [] -> raise Empty
      | [], r -> dequeue (List.rev r, [])
      | x :: q, r -> x, (q, r)
  end
```

### Information Hiding

```ocaml
module type QueueType =
  sig
    type 'a queue = 'a list (* exposes implementation of type *)
    val empty : 'a queue
    val is_empty : 'a queue -> bool
    val enqueue : 'a -> 'a queue -> 'a queue
    val dequeue : 'a queue -> 'a * 'a queue
    exception Empty
  end
```

### Modules and Separate Compilation

```ocaml
(** myQueue.mli *)

type 'a queue

exception Empty

val empty : 'a queue

val is_empty : 'a queue -> bool

val enqueue : 'a -> 'a queue -> 'a queue

val dequeue : 'a queue -> 'a * 'a queue
```

```ocaml
(** myQueue.ml *)
type 'a queue = 'a list * 'a list

exception Empty
        
let empty =
  [], []

let is_empty q =
  empty = q

let enqueue x (q, r) =
  q, x :: r
    
let rec dequeue = function
  | [], [] -> raise Empty
  | [], r -> dequeue (List.rev r, [])
  | x :: q, r -> x, (q, r)
```


### Higher-Order Modules: Functors

```ocaml
module type PrioQueueType =
  sig
    type prio = int
    type 'a prio_queue
    
    exception Empty
    
    val empty : 'a prio_queue
    val is_empty : 'a prio_queue -> bool
    val insert : 'a prio_queue -> prio -> 'a -> 'a prio_queue
    val min : 'a prio_queue -> 'a option
    val delete_min : 'a prio_queue -> 'a prio_queue
  end
```

```ocaml
module PrioQueue : PrioQueueType =
  struct
    type prio = int
    type 'a prio_queue = (prio * 'a) list

    exception Empty

    let empty = []
    let is_empty q =
      q = empty

    let insert q p v =
      let rec ins = function
        | [] -> [(p, v)]
        | (p1, v1) :: q1 ->
            if compare p p1 < 0 (* p < p1 ? *)
            then (p, v) :: q1
            else (p1, v1) :: ins q1
      in
      ins q

    let min = function
      | [] -> None
      | (_, v) :: _ -> Some v

    let delete_min = function
      | [] -> raise Empty
      | _ :: q1 -> q1
  end
```

```ocaml
module PrioQueue : PrioQueueType =
  struct
    type 'prio compare_fun = 'prio -> 'prio -> int
    type ('prio, 'a) prio_queue = ('prio * 'a) list * 'prio compare_fun

    exception Empty

    let empty compare =
      ([], compare)

    let is_empty (q, _) =
      q = []

    let insert (q, compare) p v =
      let rec ins q =
        match q with
        | [] -> [(p, v)]
        | (p1, v1) :: q1 ->
            if compare p p1 < 0 (* p < p1 ? *)
            then (p, v) :: q
            else (p1, v1) :: ins q1
      in
      ins q, compare
      
    let min = function
      | [], _ -> None
      | (_, v) :: _, _ -> Some v

    let delete_min = function
      | [], _ -> raise Empty
      | _ :: q1, compare -> (q1, compare)
  end
```

```ocaml
module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end
```

```ocaml
module MakePrioQueue (O: OrderedType) : PrioQueueType =
  struct
    type prio = O.t
    type 'a prio_queue = (prio * 'a) list

    let compare = O.compare
          
    exception Empty

    let empty = []
    let is_empty q =
      q = empty

    let insert q p v =
      let rec ins = function
        | [] -> [(p, v)]
        | (p1, v1) :: q1 ->
            if compare p p1 < 0 (* p < p1 ? *)
            then (p, v) :: q1
            else (p1, v1) :: ins q1
      in
      ins q
      
    let min = function
      | [] -> None
      | (_, v) :: _ -> Some v

    let delete_min = function
      | [] -> raise Empty
      | _ :: q1 -> q1

  end
```



```ocaml
module MakePrioQueue (O: OrderedType) : PrioQueueType with type prio = O.t =
  struct
    ...
  end
```
