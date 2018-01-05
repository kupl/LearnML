(* Department: EE
 * Student No.: 2009-20769
 * Name: Kim, Seongjun
 * Exercise 6
 *)

module type Queue =
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end

module IntListQ:Queue with type element = int list =
struct
  type element = int list
  type queue = Queue of int list * int list
  exception EMPTY_Q
  let emptyQ = Queue ([], [])
  let enQ ((q:queue), (e:element)) =
    match q with
	Queue (a, b) -> Queue (e @ a, b)

  let rec deQ (q:queue) =
    match q with
	Queue ([], []) -> raise EMPTY_Q
      | Queue (a, []) -> deQ (Queue ([], List.rev a))
      | Queue (a, (b::bs)) -> ([b], Queue (a, bs))
end

(*
let q = IntListQ.emptyQ;;
let q = IntListQ.enQ (q, [1;1;1;1]);;
let q = IntListQ.enQ (q, [2]);;
let q = IntListQ.enQ (q, [3]);;
let q = IntListQ.enQ (q, [4]);;
let q = IntListQ.enQ (q, [5]);;
let (x, q) = IntListQ.deQ q;;
let (x, q) = IntListQ.deQ q;;
let (x, q) = IntListQ.deQ q;;
let (x, q) = IntListQ.deQ q;;
let (x, q) = IntListQ.deQ q;;
let (x, q) = IntListQ.deQ q;;
let (x, q) = IntListQ.deQ q;;
let (x, q) = IntListQ.deQ q;;
*)
