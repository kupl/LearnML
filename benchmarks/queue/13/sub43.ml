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
		let enQ(q, elem) = match q with	| (lq, rq) -> (elem::lq, rq)
		let deQ(q) =
			match q with | (lq, rq) ->
				if(lq = [] && rq = []) then raise EMPTY_Q
				else if(rq = []) then (List.hd(List.rev lq), ([], List.tl(List.rev lq)))
				else (List.hd rq, (lq, List.tl rq))
	end
