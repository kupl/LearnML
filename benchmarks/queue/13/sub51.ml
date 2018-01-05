open List

module type Queue =
    sig
	    type element
		type queue
		exception EMPTY_Q
		val emptyQ : queue
		val enQ : queue * element -> queue
		val deQ : queue -> element * queue
	end


module IntListQ=
    struct
	    type element = int list
		type queue = element list *element list
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ : queue * element -> queue =
		    fun (q,ele) -> 
			    match q with 
				|(lelements ,relements) -> 
				    ((List.append [ele] lelements),relements)

		let deQ : queue -> element * queue =
		    fun q ->
			    match q with
				|(lelements,relements) ->
				    if (List.length relements)=0 then 
					    if (List.length lelements)=0 then raise EMPTY_Q
						else ((List.hd (List.rev lelements)),([],(List.tl (List.rev lelements))))
	                else (List.hd relements, (lelements, (List.tl relements)))
	end
