type 'a queue

exception Empty

val empty : 'a queue

val is_empty : 'a queue -> bool

val enqueue : 'a -> 'a queue -> 'a queue

val dequeue : 'a queue -> 'a * 'a queue

