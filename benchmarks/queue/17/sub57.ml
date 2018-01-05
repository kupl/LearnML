(*2013-11417 Lee Seok Jin CSE hw2_6*)

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
		type queue = (element list) * (element list)
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ((myQ, element): queue*element): queue = 
			match myQ with
			| (lq,rq) -> (element::lq, rq)
		let rec deQ(myQ: queue): element * queue = 
			match myQ with
			| ([],[]) -> raise(EMPTY_Q)
			| (lq, []) -> deQ([], List.rev(lq))
			| (lq, rqh::rqt)  -> (rqh, (lq, rqt))
	end


