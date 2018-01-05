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
	
	let emptyQ = ([],[])

	let enQ (q, e) =
		match q with
		| (fr,ba) -> (e::fr, ba)

	let rec deQ q = 
		match q with
		| ([],[]) -> raise EMPTY_Q
		| (fr,h::t) -> (h, (fr, t))
		| (fr,[]) -> deQ ([], (List.rev fr))
end