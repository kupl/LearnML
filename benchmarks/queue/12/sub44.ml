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
	type queue = QUEUE of element list * element list
	exception EMPTY_Q
	let emptyQ = QUEUE([], [])
	let enQ = fun (myQ, elem) -> 
		match myQ with
		| QUEUE(lstack, rstack) -> QUEUE(elem::lstack, rstack) (* push onto lstack*)
	let deQ = fun myQ ->
		match myQ with
		| QUEUE([], []) -> raise EMPTY_Q
		| QUEUE(lstack, []) -> (
			let rec popUntilNotEmpty myQ = 
				match myQ with
				| QUEUE([], rstack) -> myQ
				| QUEUE(hd::tl, rstack) -> popUntilNotEmpty (QUEUE(tl, hd::rstack)) (* pop from lstack & push onto rstack *)
			in
			match (popUntilNotEmpty myQ) with
			| QUEUE(lstack, hd::tl) -> (hd, QUEUE(lstack, tl))
			)
		| QUEUE(lstack, hd::tl) -> (hd, QUEUE(lstack, tl))
  end
  
