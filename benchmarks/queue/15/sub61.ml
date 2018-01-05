module type Queue = 
sig
type element
type queue
exception EMPTY_Q
val emptyQ: queue
val enQ: queue * element -> queue
val deQ: queue -> element * queue
end

module IntListQ =
struct
type element = int list
type queue = int list * int list
exception EMPTY_Q
let emptyQ = ([], [])
let enQ ((q : queue), (e : element)) : queue = (e @ fst q, snd q)
let rec deQ (q : queue) = match q with
| ([], []) -> raise EMPTY_Q
| ([], h::l) -> (([h] : element), (([], l) : queue))
| (la, lb) -> (deQ ([], lb @ (List.rev la)))
end

