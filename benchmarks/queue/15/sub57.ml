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
		let enQ = function (q,e) -> (match q with | (a,b) -> ([e]@a,b) )
		let deQ = function q -> (match q with 
								| ([],[]) -> raise EMPTY_Q
								| (a,[]) -> (match (List.rev a) with
											| h::t -> (h, ([],t)))
								| (a,b) -> (match b with
											| h::t -> (h, (a,t)))

										)
	end
