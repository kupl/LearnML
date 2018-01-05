module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end
	
module IntListQ : Queue with type element = int list =
	struct
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ ((l, r), x) = (x::l, r)
		let deQ (l, r) =
			match r with
				h::t -> (h, (l, t))
			| [] -> match l with [] -> raise EMPTY_Q
							| _ -> ((List.hd (List.rev l)), ([],(List.tl (List.rev l))))  
	end