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
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ : queue * element -> queue = fun (q, elt) ->
			let l = fst q in
			let r = snd q in
			(elt::l, r)
		let deQ : queue -> element * queue = fun elt ->
			match elt with
			| ([], []) -> raise EMPTY_Q
			| (l, []) -> 
				let l_rev = List.rev l in
				let elt = List.hd l_rev in
				let r = List.tl l_rev in
				(elt, ([], r))
			| (l, hd::r) -> (hd, (l,r))
	end
