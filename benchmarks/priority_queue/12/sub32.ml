type heap = EMPTY 
		  | NODE of rank * value * heap * heap 
and rank = int 
and value = int 

exception EmptyHeap 

let rank h = 
	match h with 
	| EMPTY -> -1 
	| NODE(r,_,_,_) -> r 

let shake (x,lh,rh) = 
	if (rank lh) >= (rank rh) then NODE(rank rh+1, x, lh, rh) 
	else NODE(rank lh+1, x, rh, lh) 

let rec merge(oh, nh) = 
	match oh with
	| EMPTY -> nh
	| NODE(_, ox, olh, orh) ->
		match nh with 
		| EMPTY -> oh
		| NODE(_, nx, nlh, nrh) ->
			if ox >= nx then shake(ox, olh, merge(orh, nh))
			else shake(nx, nlh, merge(nrh, oh))

let findMin h = 
	match h with 
	| EMPTY -> raise EmptyHeap 
	| NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
	| EMPTY -> raise EmptyHeap 
	| NODE(_,x,lh,rh) -> merge (lh,rh) 

