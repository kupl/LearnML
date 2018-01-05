(* KIHWAN KANG HW02-3 *)

(* PREDEFINED MODULE SIGNATURE *)
module type Queue =
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end
(* END OF PREDEFINED MODULE SIGNATURE *)

module IntListQ 
= struct
	type element = int list
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ = fun (que, elem) -> 
		(elem::(fst que), (snd que)) 
	let deQ = fun que -> 
		match que with
		|([], []) -> raise EMPTY_Q
		|(lque, []) -> 
			let lque_rev = List.rev lque in
			((List.hd lque_rev), ([], (List.tl lque_rev)))
		|(lque, rque) -> ((List.hd rque), (lque, (List.tl rque)))
end
