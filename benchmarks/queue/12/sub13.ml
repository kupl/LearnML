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
let enQ ((l, r), elem) = (elem::l, r)
let deQ queue = match queue with
([], []) -> raise EMPTY_Q
| (l, []) -> let reversedL = List.rev l in
		(List.hd reversedL, ([], List.tl reversedL))
| (l, r) -> (List.hd r, (l, List.tl r))
end
