type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h = match h with
	| EMPTY -> -1 
	| NODE(r,_,_,_) ->  r

let shake(x,lh,rh) =
	if (rank lh) >= (rank rh)
	then NODE(rank rh+1, x, lh, rh)
	else NODE(rank lh+1, x, rh, lh)

let findMin h = match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_,x,_,_) -> x

let rec  merge(a, b) = match (a, b) with
	| (EMPTY, _) -> b
	| (_, EMPTY) -> a
	| (NODE(_, av, la, ra), NODE(_, bv, lb, rb))  ->
		if av < bv
		then (match (la, ra) with
			| (EMPTY, _) -> shake(av, ra, b)
			| (_, EMPTY) -> shake(av, la, b)
			| _ -> if (findMin la) < (findMin ra) then shake(av, la, merge(b, ra))
				else shake(av, ra, merge(b, la)))
		else (match (lb, rb) with
			| (EMPTY, _) -> shake(bv, rb, a)
			| (_, EMPTY) -> shake(bv, lb, a)
			| _ -> if (findMin lb) < (findMin rb) then shake(bv, lb, merge(a, rb))
				else shake(bv, rb, merge(a, lb)))


let insert(x, h) = merge(h, NODE(0, x, EMPTY, EMPTY))

let deleteMin h = match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge(lh, rh)	
 
