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
		type queue = (element list) * (element list)
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ q_e = 
			match q_e with
			| ((q_l, q_r), elem) -> ((elem :: q_l), q_r)
		let deQ queue = 
			match queue with
			| ([], []) -> raise EMPTY_Q
			| (q_l, []) -> ((List.hd (List.rev q_l)), ([], (List.tl (List.rev q_l))))
			| (q_l, q_r) -> ((List.hd q_r), (q_l, (List.tl q_r)))
	end
