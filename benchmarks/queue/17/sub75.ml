module type Queue =
sig
	exception EMPTY_Q
	type element
	type queue
	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end

module IntListQ =
struct
	exception EMPTY_Q
	type element = int list
	type queue = int list list * int list list
	let emptyQ : queue = ([], [])
	let enQ ((q : queue), (e : element)) : queue = 
		(e::fst q, snd q)
	let deQ (q :queue) : element * queue =
		match q with
		|([],[]) -> raise EMPTY_Q
		|(_, []) -> (List.hd (List.rev (fst q)), ([], List.tl (List.rev (fst q))))
		|(_, e0::l0) -> (e0, (fst q, l0))
end