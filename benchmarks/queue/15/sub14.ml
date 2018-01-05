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
		let emptyQ : queue = ([], [])
		let enQ (q, e) : queue = 
			match q with (l, r) -> (e::l, r)
		let deQ q : element * queue =
			match q with
			  ([], []) -> raise EMPTY_Q
			| (l, []) -> 
				let l = List.rev l in
				let h = List.hd l in
				let t = List.tl l in
				(h, ([],t))
			| (l, h::t) -> (h, (l,t))
	end


module ValidIntListQ = (IntListQ : Queue)
