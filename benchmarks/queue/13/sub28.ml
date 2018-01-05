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
	type queue = (element list) * (element list)
	exception EMPTY_Q
	let emptyQ = ([], [])
	let enQ (q, e) =
	  match q with
	  | (l1, l2) -> ((e::l1), l2)

	let deQ (q: queue) =
	  match q with
	  | ([], []) -> raise EMPTY_Q
	  | (l1, l2) ->
	    if (l2 = []) then ((List.hd (List.rev l1)), ([], (List.tl (List.rev l1))))
	    else ((List.hd l2), ((l1, List.tl l2)))

  end
