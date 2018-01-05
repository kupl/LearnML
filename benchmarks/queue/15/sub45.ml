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
		type queue = int list list * int list list
		exception EMPTY_Q
		let emptyQ = ([], [])

		let enQ (que, elem) =
			match que with
			| (l, r) -> (elem::l,r)
		let deQ que =
			match que with
			| ([], []) -> raise EMPTY_Q
			| ([], hd::tl) -> (hd, ([], tl))
			| (l, []) -> (List.hd (List.rev l), ([], List.tl (List.rev l)))
			| (l, rh::rt) -> (rh, (l, rt))

	end

