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
		let emptyQ = ([],[])
		let enQ (q, e) = 
		(
			let (a, b) = q in
			(e::a, b)
		)
		let deQ q =
		(
			let (a, b) = q in
			match b with
				[] ->
				(
					let new_b = (List.rev a) in
					match new_b with
						[] -> raise (EMPTY_Q)
						|h::t -> (h,([],t))
				)
				|h::t -> (h,(a,t))
		)
	end
	