

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
	type queue = int list * int list
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ (queue, element) = 
		match queue with
		| (lq, rq) -> (element::lq, rq)
	let rec deQ queue = 
		match queue with
		| ([], []) -> raise EMPTY_Q
		| (lq, []) -> deQ(([],List.rev(lq)))
		| (lq, element::rq_remain) -> 
			(element, (lq,rq_remain))
end



