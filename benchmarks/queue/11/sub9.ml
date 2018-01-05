(* hw5. *)
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
		let emptyQ = ([], [])
		let enQ (q, e) =
			match q with
			 (q1, q2) -> (e::q1, q2)
		let rec deQ q = 
			match q with
			 ([], []) -> raise EMPTY_Q
			|(q, []) -> deQ([], List.rev q)
			|(q, hd::tl) -> (hd, (q, tl))
	end
