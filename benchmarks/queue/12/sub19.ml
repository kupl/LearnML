(*2009-11718 2-4*)

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
	type queue = (element list * element list)
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ (q, e) = 
			match q with
			|([], []) -> ([e], [])
			|(l, []) -> (e::l, [])
			|([], l) -> ([e], l)
			|(l1, l2) -> (e::l1, l2)
	let deQ q =
			match q with
			|([], []) -> raise EMPTY_Q
			|([], h::t) -> (h, ([], t))
			|(l, []) -> ((List.hd (List.rev l)), ([], (List.tl (List.rev l))))
			|(l, h::t) -> (h, (l, t))
end

