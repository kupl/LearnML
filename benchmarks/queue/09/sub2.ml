module IntListQ = 
	struct
		type element = int list
		type queue = int list list * int list list
		exception EMPTY_Q
		let emptyQ = (([], []): queue)
		let enQ = fun (((a,b):queue), (c: element)) ->((c::a,b): queue)
		let deQ = fun ((a,b): queue) -> if a=[] && b=[] then raise EMPTY_Q
							   else if b=[] then ((List.hd(List.rev(a)): element), ([], (List.tl(List.rev(a))): queue))
							   else ((List.hd(b): element), ((a, List.tl(b)): queue))

	end	
		
		
