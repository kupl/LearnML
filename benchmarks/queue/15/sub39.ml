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
	let emptyQ: queue = ([], [])
	let enQ: queue * element -> queue =
		fun (q, e) ->
		match q with
		| (el1, el2) -> ([e]@el1, el2)

	let deQ: queue -> (element * queue) = 
		fun q ->
		match q with
		| ([],[]) -> raise EMPTY_Q
		| (el, []) -> (List.hd (List.rev el), ([], (List.tl(List.rev el))))
		| (el1, el2) -> (List.hd el2, (el1, List.tl el2))
end

(*
module ValidIntListQ = (IntListQ : Queue) 
*)