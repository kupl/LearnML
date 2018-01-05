(* 2006-11377 hw2-5 *)

module type Queue = sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue 
	val deQ: queue -> element * queue
end

module IntListQ = struct
	type element = int list 
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([],[])

	let enQ (q, elem) = 
		match q with (left, right) -> (elem::left, right)

	let rec deQ q = 
		match q with 
		| ([], []) -> raise EMPTY_Q
		| (left, []) -> deQ ([], List.rev left)
		| (left, top::rest) -> (top, (left,rest))
end
