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
		let enQ (q, e) = match q with
				(l1, l2) -> (e::l1, l2)

		let deQ q = match q with
				([],[]) -> raise EMPTY_Q
				| (l1, []) -> (let rl1 = (List.rev l1) in
						match rl1 with
						h::t -> (h, ([], t))
						| [] -> raise EMPTY_Q)
				| (l1, (h::t)) -> (h, (l1, t))
	end
