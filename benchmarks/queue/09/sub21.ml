module type Queue =
  	sig
  		type element
  		type queue
  		exception EMPTY_Q
  		val emptyQ: queue
  		val enQ: queue * element -> queue
  		val deQ: queue -> element * queue
  	end;;

module IntListQ =
	struct 
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ = fun(queuein,ele) -> match queuein with
						(a,b) -> (ele::a,b)
		let deQ = fun queuein -> match queuein with
						(a,b) -> if (List.length b) = 0 then (match (List.rev a) with
												x::c -> (x,([],c))
												| _ -> raise EMPTY_Q
											)
										else (match b with
											x::c -> (x,(a,c))
											| _ -> raise EMPTY_Q
											)
	end;;