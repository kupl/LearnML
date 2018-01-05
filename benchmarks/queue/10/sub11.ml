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

	let emptyQ = ([], [])
	let enQ (q, e) = match q with (l1, l2) -> (e::l1, l2)
	let deQ q = match q with (l1, l2) ->
		let l = l2@(List.rev l1) in
		match l with
		  [] -> raise EMPTY_Q
		| h::t -> (h, ([], t))
end