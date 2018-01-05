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
type queue = element list * element list
exception EMPTY_Q
let emptyQ = ([], [])
let enQ (q, e) = 
	match q with
	| (a, b) -> (e::a, b)
let deQ q =
	match q with
	| ([], []) -> raise EMPTY_Q
	| ([], (hd::tl)) -> (hd, ([], tl))
	| (l, []) -> (List.hd (List.rev l), ([], List.tl (List.rev l)))
	| (l1, (hd::tl)) -> (hd, (l1, tl))
end
