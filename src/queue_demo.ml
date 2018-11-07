module type QueueType =
  sig
    type 'a queue
    val empty : 'a queue
    val is_empty : 'a queue -> bool
    val enqueue : 'a -> 'a queue -> 'a queue
    val dequeue : 'a queue -> 'a * 'a queue
    exception Empty
  end


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

let q = Queue.empty

let flip f x y = f y x
    
let q1 =
  List.fold_left
    (flip Queue.enqueue)
    Queue.empty
    [1; 2; 3; 4]

let v, _ = Queue.(empty |> enqueue 1 |> enqueue 2 |> dequeue)

let _ = Printf.printf "%d\n" v

let zero =
  let module M = struct
    let i = 0
  end
  in
  M.i
