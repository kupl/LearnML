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
		let getH (a,b) = a
		let getT (a,b) = b
		let enQ (q, elt) = (elt::(getH q), getT q)
		let deQ q = 
			if (q = emptyQ) then raise EMPTY_Q
			else
				match (getT q) with
					[] -> let deQ_in rev_H = 
						  	match rev_H with
								[] -> raise EMPTY_Q
							  | h::t -> (h, ([],t))
							in
								deQ_in (List.rev (getH q))
				   | h'::t' -> (h',(getH q, t')) 
	end

