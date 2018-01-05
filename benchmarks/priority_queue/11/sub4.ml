type heap = EMPTY | NODE of rank * value * heap * heap 
and rank = int 
and value = int 

exception EmptyHeap 

let rank h = match h with 
	| EMPTY -> -1 
  	| NODE(r,_,_,_) -> r 

let shake (x,lh,rh) = 
	if (rank lh) >= (rank rh) 
	then NODE(rank rh+1, x, lh, rh) 
	else NODE(rank lh+1, x, rh, lh) 

let rec merge (lh,rh) =
	match (lh,rh) with
	| (EMPTY, _) -> rh
	| (_, EMPTY) -> lh
	| (NODE(r1,x1,lh1,rh1),NODE(_,x2,_,_)) ->
		if x1>x2 then merge(rh,lh)
		else (shake (x1,lh1, merge(rh1,rh)))

let findMin h = match h with 
	| EMPTY -> raise EmptyHeap 
	| NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
	| EMPTY -> raise EmptyHeap 
	| NODE(_,x,lh,rh) -> merge (lh,rh) 

let h1 = NODE(0,3,EMPTY,EMPTY)
let h2 = NODE(1,2,NODE(0,7,EMPTY,EMPTY),NODE(0,4,EMPTY,EMPTY))
