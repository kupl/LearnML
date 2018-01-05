module type Queue =
	sig
		type element
		type queue
		exception EMPTYQ
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end

module IntListQ : Queue with type element = int list = 
struct 
	type element = int list
	type queue = element list * element list
	exception EMPTYQ
	let emptyQ = ([],[])
	let enQ (q,e) =
		match q with
			| (f,s) -> (e::f, s)
	let rec deQ q =
		match q with
			| ([], []) -> raise EMPTYQ
			| (f, []) -> deQ ([], List.rev f)
			| (f,h::t) -> (h, (f, t))
end 
