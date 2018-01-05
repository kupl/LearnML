(* 2009-13384 CHO Hyunik *)



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
	type queue = Q of element list * element list
	exception EMPTY_Q
	let emptyQ = Q([],[])
	let enQ (que,ele) =
		match que with
		Q(a,b) -> Q(ele::a,b)
	let rec deQ que =
		match que with
		Q([],[]) -> raise EMPTY_Q
		| Q(a,[]) -> deQ (Q([], List.rev a))
		| Q(a,b::c) -> (b, Q(a,c))
end