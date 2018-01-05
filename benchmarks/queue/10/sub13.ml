
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
		type queue = (element list * element list)
		exception EMPTY_Q
		let emptyQ = ([],[])
		let enQ (theq, thel) =
			(([thel] @ (fst theq)) , (snd theq))
		let rec deQ theq =
			if (theq = emptyQ) then raise EMPTY_Q
			else(
			match (snd theq) with
				[] -> let newQ = ([], (List.rev (fst theq))) in
					(deQ newQ)
				| (h::t) -> (h, ((fst theq),t)))
	end	

