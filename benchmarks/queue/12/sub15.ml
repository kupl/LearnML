(* ex4 *)
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
		
		let enQ ((inQ, outQ), elem) = 
			if outQ = [] then ([], List.rev (elem::inQ))
			else (elem::inQ, outQ)
		
		let deQ (inQ, outQ) = 
			if outQ = [] then raise EMPTY_Q
			else if (List.length outQ) = 1 then (List.hd outQ, ([], List.rev inQ))
			else (List.hd outQ, (inQ, List.tl outQ))
	end