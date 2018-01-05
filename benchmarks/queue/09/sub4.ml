module IntListQ =
	struct
		type element = int list
		type queue = (element list * element list)
		exception EMPTY_Q
		let emptyQ = (([],[]):queue)
		let enQ((a:queue), (b:element)) =
			match a with
			(x, y) -> ((b::x, y):queue)
		let rec deQ (a:queue) =
			match a with
			([], []) -> raise EMPTY_Q
			|(x, []) -> deQ(([], (List.rev x)))
			|(x, h::t) -> (h, ((x, t):queue))
		
	end
