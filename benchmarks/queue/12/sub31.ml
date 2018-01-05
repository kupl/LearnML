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
	let enQ (q,e) = match q with
		| (s1,s2) -> (e::s1,s2)
	let deQ q = match q with
		| ([],[]) -> raise EMPTY_Q
 		| (s1,[]) -> (match (List.rev s1) with
				| hd::tl -> (hd, ([],tl)) )
		| (s1,s2) -> (match s2 with 
				| hd::tl -> (hd,(s1,tl)))
	end
