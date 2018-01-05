
type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h =
	match h with 
	| EMPTY -> -1
	| NODE(r,_,_,_) -> r 

let shake (x,lh,rh) =
	if (rank lh) >= (rank rh)
	then NODE(rank rh+1, x, lh, rh)
	else NODE(rank lh+1, x, rh, lh)

let rec merge (lh, rh) =
	match (lh, rh) with
	| (EMPTY, EMPTY) -> EMPTY
	| (EMPTY, h) | (h, EMPTY) -> h
	| (NODE (lh_r, _, lh_lh, lh_rh), NODE (rh_r, _, rh_lh, rh_rh)) ->
		if lh_r <= rh_r
		then shake (lh_r, lh_lh, merge (lh_rh, rh))	
		else shake (rh_r, rh_lh, merge (lh, rh_rh))
		
let findMin h =
	match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_,x,_,_) -> x

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))
	
let deleteMin h = match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge (lh,rh) 


