module type Queue = 
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end

module IntListQ : Queue with type element = int list = 
	struct 
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ  = fun (q, e) -> match q with (l1, l2) -> (e::l1, l2)
		let deQ = fun q -> match q with ([], []) -> raise EMPTY_Q
						|(l1, []) -> (let l2 = List.rev l1 in
								(List.hd l2, ([], List.tl l2)))
						|(l1, h::t) -> (h, (l1, t))
	end