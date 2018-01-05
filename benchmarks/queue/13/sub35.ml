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
		let emptyQ = ([],[])
		let enQ = fun (q, e) ->
			match q with
			(l, r) -> (e::l, r)
		let rec deQ = fun (q) ->
			match q with
			([],[]) -> raise EMPTY_Q
			| (l, r) -> (
				match r with
					[] -> deQ ([], List.rev l)
					| h::t -> (h, (l,t))
				)
	end
