module type Queue =
sig
	type element = int list
	type queue = element * element
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end;;

module IntListQ :Queue=
struct
	type element = int list
	type queue = element * element
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ (q, e) = 
		match q with
			(e1,e2) -> (e@e1, e2)
	let deQ q =
		match q with
			([], []) -> raise EMPTY_Q
			| (e1, []) -> ([List.hd (List.rev e1)], ([], List.tl (List.rev e1)))
			| (e1, e::e2) -> ([e], (e1, e2)) 
end;;
