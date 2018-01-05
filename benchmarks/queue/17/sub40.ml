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
		type queue = (int list) list * (int list) list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ (q, e) = match q with
			| (left, right) -> (e::left, right)
		let rec deQ q = match q with
			| ([],[]) -> raise EMPTY_Q
			| (left, []) -> deQ ([], List.rev left)
			| (left, hd::tl) -> (hd, (left, tl))
	end
