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
	let enQ ((l,r), a) = (a::l,r)
	let deQ (l,r) = if (r = []) && (l = []) then raise EMPTY_Q 
			else if r = [] then ((List.hd (List.rev l)), ([],(List.rev (List.tl (List.rev l)))))
			else ((List.hd r), (l,(List.tl r)))
	end
