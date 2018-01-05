module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ : queue
		val enQ : queue * element -> queue
		val deQ : queue -> element * queue
	end

module IntListQ =
	struct
		type element = int list
		type queue = Pair of element list * element list
		exception EMPTY_Q
		let emptyQ : queue = Pair([],[])
		let enQ ((q : queue), (l : element)) : queue = match q with
												Pair(list1,list2) -> Pair([l]@list1,list2)
		let rec deQ (q : queue) : element * queue = match q with
									Pair(list1,hd::tl) -> (hd,Pair(list1,tl))
									|Pair(hd::tl,[]) -> deQ(Pair([],List.rev ([hd]@tl)))
									|Pair([],[]) -> raise EMPTY_Q
	end

