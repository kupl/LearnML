(* 200511843 LEE JONGHO *)


module type Queue =
	sig
		type element
		type queue
		exception EMPTY_Q
		val emptyQ: queue
		val enQ: queue * element -> queue
		val deQ: queue -> element * queue
	end

module IntListQ:Queue with type element = int list =
	struct
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ (q, e) =
			match (q, e) with
			(queue, element) -> match queue with (l, r) -> (e::l, r)
		let deQ q =
			let rec reverse l =
				match l with
				[] -> []
				| h::t -> (reverse t)@[h] in
			let head l =
				match l with
				[] -> raise EMPTY_Q
				| h::t -> h in
			let tails l =
				match l with
				[] -> raise EMPTY_Q
				| h::t -> t in				  
			match q with queue -> match queue with
			(l, r) -> match (l, r) with
				  ([], []) -> raise EMPTY_Q
			 	  | (h::t, []) -> (head (reverse (h::t)), ([], tails (reverse (h::t))))
				  | (_, h::t) -> (h, (l, t))
	end