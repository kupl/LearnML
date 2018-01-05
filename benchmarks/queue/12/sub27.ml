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
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ (q, ele ) =
			(match q with 
			| (lq, rq) 	-> (ele::lq, rq)
			)
		let deQ q =
			match q with
			| ([], []) 		-> raise EMPTY_Q
			| (lq, hd::tl)	-> (hd, (lq, tl))
			| (lq, []) 		-> (let lst = (List.rev lq) in ((List.hd lst), ([], (List.tl lst))))
end


