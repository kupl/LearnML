(* ex6 Queue made with two Stack *)
module type Queue = 
	sig
		type element
		type queue
		exception	EMPTY_Q
		val emptyQ : queue
		val enQ : queue * element -> queue
		val deQ : queue -> element * queue
	end


module IntListQ : Queue with type element = int list =
	struct
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ( [], [] )
		let enQ = fun ( ( listL, listR ), ele ) -> ( ele::listL, listR )
		let deQ = fun que -> 
					match que with
						( [], [] ) -> raise EMPTY_Q
						| ( listL, listR ) ->
							( match listR with
							  	[] ->
									( let nlistR = List.rev listL in 
									  ( ( List.hd nlistR ), ( [], ( List.tl nlistR ) ) ) )
								| hd::tl -> ( hd, ( listL, tl ) ) )
	end
	
