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
		
		let emptyQ: queue = ([], [])
		
		let enQ ((q: queue), (e: element)): queue =
			match q with
			| (lil, ril) -> (e::lil, ril)
		
		let rec deQ (q: queue): element * queue =
			match q with
			| ([], []) -> raise EMPTY_Q
			| (lil, []) -> deQ ([], List.rev lil)
			| (lil, h::t) -> (h, (lil, t))
	end
