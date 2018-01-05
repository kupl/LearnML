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
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ ((q: queue), (e: element)) : queue =
			((List.append [e] (fst q)), snd q)
		let rec deQ (q: queue) : element * queue  =
			match (snd q) with
			| sh::st -> (sh, (fst q, st))
			| [] ->
				(match (fst q) with
				| [] -> raise EMPTY_Q
				| fh::ft ->
					let rec listReverse (l: element list) : element list =
						match l with
						| [] -> []
						| h::t -> (List.append (listReverse t) [h])
						in
					deQ([], listReverse(fst q))
				)
	end

			
			
