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
		type queue = int list * int list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ (q, e) = (List.append e (fst q), snd q)
		let rec deQ q = 
			match q with
			| ([], []) -> raise EMPTY_Q
			| (l, []) -> deQ ([], List.rev l)
			| (l, r) -> ((List.hd r)::[], (l, List.tl r))
	end