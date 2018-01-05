(* 2009-11679 김정명 2-5 *)

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
		type queue = int list list * int list list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ (q, e) =
			match q with
				(h, t) -> (e::h, t)
		let rec deQ q =
			match q with
			  ([], []) -> raise EMPTY_Q
			| (l, []) -> deQ ([], List.rev l)
			| (l, h::t) -> (h, (l, t))
	end
