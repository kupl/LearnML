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
		let enQ (q, item) = match q with
			| (q1, q2) -> (item :: q1, q2)
		let rec deQ q = match q with
			| ([], []) -> raise EMPTY_Q
			| (q1, []) -> deQ ([], List.rev q1)
			| (q1, hd2 :: tail2) -> (hd2, (q1, tail2))
	end
