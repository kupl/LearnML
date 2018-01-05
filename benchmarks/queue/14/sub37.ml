(* not tested *)

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
		type queue = Queue of element list * element list 
		exception EMPTY_Q

		let emptyQ = Queue ([], [])

		let enQ ((q: queue), (elt: element)) = 
			match q with
			| Queue (l, r) -> Queue ([elt]@l, r)

		let deQ (q: queue) = 
			match q with
			| Queue ([], []) -> raise EMPTY_Q
			| Queue (l, []) -> (List.hd (List.rev l)), Queue ([], List.tl (List.rev l))
			| Queue (l, r) -> (List.hd r), Queue (l, List.tl r)
	end
