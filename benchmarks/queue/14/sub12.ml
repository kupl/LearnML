module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ : queue
		val enQ : queue * element -> queue
		val deQ : queue -> element * queue
	end
;;

module IntListQ = 
  	struct
  		type element = int list
  		type queue = Queue of element * element
  		exception EMPTY_Q
  		let emptyQ = Queue ([], [])
  		let rec enQ (q,e) = 
  			match e with
  			[] -> q
  			| h :: t -> match q with
  				Queue (first, second) -> Queue (List.rev_append e first, second)
  		let rec deQ q =
  			match q with
  			Queue (first, second) -> match second with
						[] ->	(match first with
							[] -> raise (EMPTY_Q)  							
			  				| _ -> deQ (Queue ([], List.rev first)))
  						| h :: t -> ( [h], Queue (first, t) )
  	end
;;
