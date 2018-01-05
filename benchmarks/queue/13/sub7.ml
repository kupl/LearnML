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
		let enQ = fun (queue, element) ->
			match queue with
			| (l, r) -> (element::l, r)
		let rec deQ = fun queue ->
			match queue with
			| ([], []) -> raise EMPTY_Q
			| (l, []) -> deQ ([], List.rev l)
			| (l, r) -> ((List.hd r), (l, List.tl r))
			
	end