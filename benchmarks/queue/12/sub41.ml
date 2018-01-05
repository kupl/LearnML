module type Queue = 
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ : queue * element -> queue
		val deQ : queue -> element * queue
	end

module IntListQ = 
	struct 
		type element = int list
		type queue = element * element
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ (a, b) = match a with
			| (q1, q2) -> b::q1
		let rec deQ a = match a with
			| ([],[]) -> raise EMPTY_Q
			| (q1, []) -> deQ ([], q1)
			| (q1, q2) ->( (q1, (List.rev (List.tl (List.rev q2)))) , (List.nth (List.rev q2) 0) )
	end
			
