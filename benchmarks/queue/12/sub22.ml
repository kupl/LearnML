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
		type queue = element list * element list
		exception EMPTY_Q
		
		let emptyQ = ([], [])
		
		let enQ (q, e) =
			match q with
			(l, r) -> (e::l, r)

		let deQ q =
			match q with
			([], []) -> raise EMPTY_Q
			|(l, []) -> (
				let revL = List.rev l in
				(List.hd revL, ([], List.tl revL))
			)
			|(l, hd::tl) -> (hd, (l, tl))
	
	end
