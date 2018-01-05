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
		let enQ ((q: queue), (el: element)): queue =
			match q with
			| (l, r) -> (el::l, r)
		let deQ (q: queue): (element * queue) = 
			match q with
			| (l, h::t) -> (h, (l, t))
			| (l, []) -> 
				match (List.rev l) with
				| [] -> raise EMPTY_Q
				| h::t -> (h, ([], t))	
	end

