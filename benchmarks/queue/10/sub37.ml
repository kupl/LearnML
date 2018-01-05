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
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ = function ((is, os), e) -> (e::is, os)
	let deQ= function ([], []) -> raise EMPTY_Q
		| (is, []) -> let h::t = List.rev is in 
			(h, ([], t))
		| (is, h::t) -> (h, (is,t))
end

