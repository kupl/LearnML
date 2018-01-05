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
	type queue = Q of int list list * int list list
	exception EMPTY_Q
	let emptyQ = Q([], [])
	let enQ (q, e) = 
		match (q, e) with
		| (Q(l, r), e) -> Q([e] @ l, r)
	let rec deQ q = 
		match q with
		| Q([], []) -> raise EMPTY_Q
		| Q(l, []) -> deQ(Q([], List.rev l))
		| Q(q1, a::q2) -> (a, Q(q1, q2))
 end
