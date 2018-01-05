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
	type queue = ((int list) list * (int list) list)
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ ((l1,l2),e) = (e::l1,l2) 
	let deQ q = match q with
			| (l1,[]) -> (match (List.rev l1) with 
						[] -> raise EMPTY_Q
						| e::[] -> (e,([],[]))
						| e::left -> (e,([],left)))
			| (l1,e::l2) -> (e,(l1,l2))
end
