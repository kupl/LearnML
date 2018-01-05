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
	let emptyQ : queue = ([], [])
	let enQ ((q: queue), (e: element)): queue =
		match q with
		| (q1, q2) -> (e::q1, q2)
	let deQ (q: queue): (element * queue) =
		match q with
		| ([], []) -> raise EMPTY_Q
		| (q1, []) -> (let rev_q1 = List.rev q1 in
			(List.hd rev_q1, ([], (List.tl rev_q1))))
		| (q1, q2) -> (List.hd q2, (q1, List.tl q2))
end

(*
module ValidIntListQ = (IntListQ : Queue)
*)