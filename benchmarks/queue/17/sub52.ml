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
	type queue = Queue of element list * element list
	exception EMPTY_Q
	let emptyQ = Queue ([], [])
	let enQ (q, e) =
		match q with
		| Queue (a, b) -> Queue (e :: a, b)

	let rec deQ q =
		match q with
		| Queue ([], []) -> raise EMPTY_Q
		| Queue (a, []) -> deQ (Queue ([], List.rev a))
		| Queue (a, (h::b)) -> (h, Queue (a, b))
end
