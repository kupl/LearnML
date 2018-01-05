
(* ex 5 *)

module type Queue = 
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end

module IntListQ : Queue with type element=int list = 
	struct
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([],[]) 
		let enQ(q,e) = 
			match q with
			| (lq,rq) -> (e::lq,rq)

		let rec deQ q =
			let rec reverse q = 
				match q with	
				| h::t -> (reverse t) @ [h]
				| [] -> []	
 			in
 
			match q with
			| ([],[]) -> raise EMPTY_Q
			| (lq,[]) -> deQ([],(reverse lq))
			| (lq,h::t) -> (h,(lq,t))
	end




