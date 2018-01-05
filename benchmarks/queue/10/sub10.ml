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
		type queue = (element list) * (element list)
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ ((a, b), e) = (e::a, b)
		let rec deQ q = 
			match q with
			([], []) -> raise EMPTY_Q
			| (a, h::t) -> (h, (a, t))
			| (a, []) -> deQ ([], List.rev a)
	end

