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
		let enQ (que, elem) = (elem::(fst que), snd que)
		let deQ que =
			match que with
			| ([],[]) -> raise EMPTY_Q
			| (l1,[]) ->
				let lst = List.rev l1 in
				((List.hd lst), ([], List.tl lst))
			| (l1,l2) ->
				((List.hd l2), (l1, List.tl l2))
end
