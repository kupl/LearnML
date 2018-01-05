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
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ (queue, element) =
		match queue with
		([], r) -> ([element], r)
		| (l, []) -> (List.append [element] l, [])
		| (l, r) -> (List.append [element] l, r)
	let deQ queue =
		match queue with
		([], []) -> raise(EMPTY_Q)
		| (l, []) -> (List.hd (List.rev l), ([], List.tl (List.rev l)))
		| (l, h::t) -> (h, (l, t))
end