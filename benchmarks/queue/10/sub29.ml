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
		type queue = int list list * int list list
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ (q,x) = match q with (r,l)->(x::r,l)
		let deQ q = match q with ([],[])->raise EMPTY_Q
								|(r,[])->(let newr = List.rev r in (List.hd newr,([],List.tl newr)))
								|(r,h::t)->(h,(r,t))	
	end
