module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ : queue
		val enQ : queue * element -> queue
		val deQ : queue -> element * queue
	end
module IntListQ : Queue with type element = int list =
	struct
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q

		let emptyQ = ([], [])
		let enQ (q, el) =
			match q with
			| (a, b) -> (el::a, b)
		let rec deQ q =
			match q with
			| ([], []) -> raise EMPTY_Q
			| (a, []) -> deQ ([], (List.rev a))
			| (a, hd::tl) -> (hd, (a, tl))
	end

