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
		type queue = EMPTY | QUEUE of element list * element list
		exception EMPTY_Q
		let emptyQ = EMPTY
		let enQ(queue, element) = match queue with
			EMPTY -> QUEUE(element::[], [])
			| QUEUE(a, b) -> QUEUE(element::a, b)
		let rec deQ(queue) = match queue with
			EMPTY -> raise EMPTY_Q
			| QUEUE(a, []) -> deQ(QUEUE([], List.rev a))
			| QUEUE([], h::b) -> 
			(
			 	if b = [] then (h, EMPTY)
				else (h, QUEUE([], b))
			)
			| QUEUE(a, h::b) -> (h, QUEUE(a, b))
			 	
	end 

