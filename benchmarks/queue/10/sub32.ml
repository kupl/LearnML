module type Queue =
	sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ : queue
	val enQ : queue * element -> queue
	val deQ : queue -> element * queue
	end 

module IntListQ =
	struct
	type element = int list
	type queue = int list list * int list list
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ (queue, element) =
		match queue with
		(lst1,lst2) -> (element::lst1, lst2)
	let rec deQ queue = 
		match queue with
		([],[]) -> raise EMPTY_Q
		|(lst1, []) -> deQ ([], List.rev lst1)
		|(lst1, h::t) -> (h, (lst1, t))
	end 
