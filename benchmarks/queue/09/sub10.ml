(*2006 11720 2-6 KimEunSol*)
module IntListQ =
	struct 
		type element = int list
		type queue = element list * element list
		exception EMPTY_Q
		let emptyQ = ([], [])
		let enQ((a,b), c) = (c::a, b)
		let deQ(a,b) = if a=[] && b=[] then raise (EMPTY_Q)
						else if b=[] then (List.hd(List.rev(a)), ([],List.tl(List.rev(a))))
						else (List.hd(b)(*:element*), (a,List.tl(b)))
end

