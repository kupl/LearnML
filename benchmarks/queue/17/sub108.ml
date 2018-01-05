(*Computer Science Engineering 2015-12683 Kim Jaein*)
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
		let enQ ((q:queue), (elem:element)) =
			match q with
			|(lq, rq) -> (elem::lq,rq)

		let deQ (q:queue) =
			match q with
			|(lq, rq) ->
				(match rq with
				|[] ->
					(match lq with
					|[] -> raise EMPTY_Q 
					| _ -> (List.hd (List.rev lq), (rq, List.rev (List.tl (List.rev lq)))))
				| _ -> ((List.hd (List.rev rq)), (lq, List.rev (List.tl (List.rev rq)))))
	end

