
module IntListQ = struct 
	type element = int list
	type queue = element list * element list
	exception EMPTY_Q
	let emptyQ = ([],[])
	let enQ((q:queue), (elem:element)) = match q with 
					   | (l,r) -> (elem::l,r)

	let rec deQ((q:queue)) = match q with
			       | ([],[]) -> raise EMPTY_Q
			       | (l,[]) -> deQ([], List.rev(l))
			       | (l, h::r) -> (h, (l,r))
end 