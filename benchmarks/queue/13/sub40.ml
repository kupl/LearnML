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

		let enQ ((a, b), e) =
			(e::a, b)

		let rec deQ q =
			match q with
				| ([], []) -> raise EMPTY_Q
				| (a, []) -> deQ ([], List.rev a)
				| (a, head::tail) -> (head, (a, tail))
				
	end
