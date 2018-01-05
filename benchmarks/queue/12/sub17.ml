(* HW2 Exercise4 2009-11697 Kim HyunJoon *)
(* Queue = 2 Stacks *)

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
	let enQ : queue * element -> queue =
		fun (q, e) -> (e::(fst q), (snd q))
	let rec deQ : queue -> element * queue =
		fun q ->
		match q with
		| ([], []) -> raise EMPTY_Q
		| (aq, []) -> deQ ([], List.rev aq)
		| (aq, hd::tl) -> (hd, (aq, tl))
end
