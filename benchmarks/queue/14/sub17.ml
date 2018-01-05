(*2009-11718 박준상 2-5*)

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
		| ([], []) -> ([e], [])
		| ([], l2) -> ([e], l2)
		| (l1, []) -> (e::l1, [])
		| (l1, l2) -> (e::l1, l2)
	let deQ q =
		match q with
		| ([], []) -> raise EMPTY_Q
		| ([], hd::tl) -> (hd, ([], tl))
		| (l, []) -> (List.hd (List.rev l), ([], List.tl (List.rev l)))
		| (l, hd::tl) -> (hd, (l, tl))
end


(*complete*)
