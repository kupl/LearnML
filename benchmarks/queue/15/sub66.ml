module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ : queue
		val enQ : queue * element -> queue
		val deQ : queue -> element * queue
	end;;

module IntListQ =
	struct
		type element = int list
		type queue = int list list * int list list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ (q, e) = 
			let (list1, list2) = q in
			([e] @ list1, list2)
		let deQ (q) = 
			if q = emptyQ then raise (EMPTY_Q)
			else 
				let (list1, list2) = q in
				match list2 with
				| [] -> let head::tail = (List.rev list1) in
						(head, ([], tail))						
				| head::tail -> (head, (list1, tail))
	end;;

