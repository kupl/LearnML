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
		type queue = (element list * element list)
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ ((lQ, rQ), e) = (e::lQ, rQ)
		let rec deQ _Q =
			match _Q with
			([], []) -> raise EMPTY_Q
			| (lQ, []) -> deQ ([], List.rev lQ)
			| (lQ, h::t) -> (h, (lQ, t))
	end
