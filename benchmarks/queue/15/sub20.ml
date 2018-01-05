module type Queue = sig
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
	type queue = QUEUE of element list * element list
	exception EMPTY_Q
	let emptyQ = QUEUE([], [])
	let rec enQ (q, e) =
		match q with
		QUEUE(inl, outl)	-> QUEUE(e::inl, outl)
	let rec deQ q = 
		match q with
		QUEUE([], [])	-> raise EMPTY_Q
		|QUEUE(inl, [])	-> deQ (QUEUE([], List.rev inl))
		|QUEUE(inl, e::outl)	-> (e, QUEUE(inl, outl))
end
