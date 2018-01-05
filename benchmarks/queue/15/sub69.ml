module type Queue = sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ : queue
	val enQ : queue * element -> queue
	val deQ : queue -> element * queue
end;;

module IntListQ = struct
	type element = int list
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ ((l, r), e) = (e::l, r)
	let rec deQ = function
		| ([], []) -> raise (EMPTY_Q)
		| (l, []) -> deQ ([], List.rev l)
		| (l, e::r) -> (e, (l, r))
end;;
