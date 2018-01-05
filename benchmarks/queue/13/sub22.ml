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
	type queue = (int list list) * (int list list)
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ (myQ, l) = (l:: (fst myQ), snd myQ)
	let deQ myQ =
		match myQ with
		| ([], []) -> raise EMPTY_Q
		| (fst, []) -> (List.hd (List.rev fst), ([], List.tl (List.rev fst)))
		| _ -> (List.hd (snd myQ), (fst myQ, List.tl (snd myQ)))
end