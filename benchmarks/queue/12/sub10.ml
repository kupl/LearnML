module type Queue =
	sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue*element -> queue
	val deQ: queue -> element*queue
	end

module IntListQ =
struct
	type element = int list
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ(q, e) =
		match q with
		| (l1, l2) -> (e::l1, l2)
	
	let deQ q =
		match q with
		| ([], []) -> raise EMPTY_Q
		| (l1, []) -> (List.hd (List.rev l1), ([], List.tl (List.rev l1)))
		| (l1, h::t) -> (h, (l1, t))
end

module ValidIntListQ = (IntListQ: Queue)
