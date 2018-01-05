module type Queue =
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end

module IntListQ : Queue with type element = int list =
struct
	type element = int list
	type queue = int list list * int list list
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ (q, x) =
		match q with
		(l, r)->(x::l, r)
	let deQ q =
		match q with
		([], []) -> raise EMPTY_Q
		|(l, []) -> ((List.nth (List.rev l) 0), ([], List.tl (List.rev l)))
		|(l, rh::rt) -> (rh, (l, rt))
end
