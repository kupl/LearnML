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
type queue = ((int list) list) * ((int list) list)
exception EMPTY_Q
let emptyQ = ([],[])
let enQ = fun ((l1,l2), e) -> (e::l1 , l2)
let deQ = fun ((l1,l2)) -> 
  if l1 = [] && l2 = [] then raise EMPTY_Q
  else if l2 = [] then (List.hd (List.rev l1), (List.rev(List.tl (List.rev l1)), []))
  else ((List.hd l2), (l1, List.tl l2))
end