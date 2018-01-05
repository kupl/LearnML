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
		let emptyQ = ([],[])
		let enQ = function (q , e) ->
			match q with 
			| (l,r) -> (e::l, r)

		let deQ = function q ->
			match q with
			| ([],[]) -> raise EMPTY_Q
			| (l,r) -> let new_r = List.append r (List.rev l) in
					   (List.hd new_r, ([], List.tl new_r))
	
	end
