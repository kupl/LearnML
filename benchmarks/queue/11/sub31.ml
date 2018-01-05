module type Queue =
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end

module IntListQ: Queue with type element = int list =
struct
	type element = int list
	type queue = ((int list) list) * ((int list) list)
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ (q, x) =
		match q with
		(y, z) -> (x::y, z)
	let deQ q =
		match q with
		(y, []) -> (match (List.rev y) with 
				h::t -> (h, ([], t) )
				| [] -> raise(EMPTY_Q) )
		| (y, h::t) -> (h, (y, t))
end
