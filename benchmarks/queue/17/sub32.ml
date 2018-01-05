module type Queue = 
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ : queue
	val enQ : queue * element -> queue
	val deQ : queue -> element * queue
end

module IntListQ =
struct
	type element = int list
	type queue = element list * element list
	exception EMPTY_Q

	let emptyQ = ([], [])

	let enQ = fun (q, elem) ->
		match q with
		| (a, b) -> (elem::a, b)

	let deQ = fun q ->
		match q with
		| ([], []) -> raise (EMPTY_Q)
		| (a, []) -> 
			(let temp = List.rev a in
			 ((List.hd temp), ([], (List.tl temp))))
		| (a, head::b) -> (head, (a, b))
end

