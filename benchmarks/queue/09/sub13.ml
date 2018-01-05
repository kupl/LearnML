(* 컴퓨터공학부/2006-11855/정용혁/HW2-ex6 *)

module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
   val deQ: queue -> element * queue
  end

module IntListQ : Queue with type element = int list =
  struct
    type element = int list
    type queue = element list * element list
    exception EMPTY_Q
    let emptyQ = (([], []) : queue)
    let enQ ((q : queue), (e : element)) = match q with (l, r) -> ((e::l, r) : queue)
    let rec deQ (q : queue) = match q with
	    ([], []) -> raise EMPTY_Q
	  | (l, h::t) -> ((h : element), ((l, t) : queue))
	  | (l, []) -> deQ (([], (List.rev l) : queue))
  end
