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
	let emptyQ = ([], [])
	let enQ ((lq, rq), ele) = (ele::lq, rq)
	let rec deQ (lq, rq) = 
		match rq with
		  [] -> if lq = [] then raise EMPTY_Q else deQ ([], List.rev lq)
		| h::t -> (h, (lq, t))
  end
