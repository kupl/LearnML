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
	type queue = QUEUE of (element list) * (element list)
	exception EMPTY_Q
	
	let emptyQ = QUEUE ([], [])
	
	let enQ: queue * element -> queue =
		function (QUEUE (l, r), e) -> QUEUE (e::l, r)
	
	let deQ (q: queue) : element * queue =
		match q with
		| QUEUE ([], []) -> raise EMPTY_Q
		| QUEUE (left, []) ->
			let newR = List.rev left in
			(List.hd newR, QUEUE ([], List.tl newR))
		| QUEUE (left, h::right) -> (h, QUEUE (left, right))

end