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
	type queue = int list list * int list list
	exception EMPTY_Q
	let emptyQ = ([],[])

	let enQ (que, elem) =
		match que with (inStk, outStk) -> (elem::inStk,outStk)
	
	let rec deQ que = match que
		with ([],[]) -> raise EMPTY_Q
		| (inStk, []) -> deQ ([], List.rev inStk)
		| (inStk, head::rest) -> (head, (inStk,rest))
end
