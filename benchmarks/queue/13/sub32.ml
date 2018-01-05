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
		let enQ (q, x) =
			match q with
			| (hd::tl, q2) -> ([x] @ hd::tl, q2)
			| ([], q2) -> ([x], q2)
		let rec deQ q =
			match q with
			| ([], []) -> raise EMPTY_Q
			| (q1, []) -> deQ ([], List.rev q1)
			| (q1, hd::tl) -> (hd, (q1, tl))
	end

