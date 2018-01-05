type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int
exception EmptyHeap

let rank h = match h with
	|EMPTY -> -1
	|NODE(r,_,_,_) -> r

let shake (x, lh, rh) =
	if (rank lh) >= (rank rh)
	then NODE(rank rh+1, x, lh, rh)
	else NODE(rank lh+1, x, rh, lh)

let rec merge (h1, h2) =
	if h1=EMPTY then h2
	else if h2=EMPTY then h1
	else
	match h1 with NODE(r1, x, lh1, rh1) ->
	match h2 with NODE(r2, y, lh2, rh2) ->
	if x>=y then
		if lh2=EMPTY then NODE(r2, y, h1, rh2)
		else if rh2=EMPTY then shake (y, lh2, h1)
		else if (rank lh2)>(rank rh2) then shake (y, lh2, merge(h1, rh2))
		else NODE(r2, y, merge(h1, lh2), rh2)
	else
		if lh1=EMPTY then NODE(r1, x, h2, rh1)
		else if rh1=EMPTY then shake (x, lh1, h2)
		else if (rank lh1)>(rank rh1) then shake (x, lh1, merge(h2, rh1))
		else NODE(r1, x, merge(h2, lh1), rh1)

let findMin h = match h with
	|EMPTY -> raise EmptyHeap
	|NODE(_,x,_,_) -> x

let insert (x, h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin h = match h with
	|EMPTY -> raise EmptyHeap
	|NODE(_,x,lh,rh) -> merge (lh, rh)

