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

let rec merge (h1, h2) =
	match (h1,h2) with
	| (EMPTY , EMPTY) -> raise EmptyHeap
	| (EMPTY , NODE(_,y,lh2,rh2))->shake (y,h2,h1)
	| (NODE(_,x,lh1,rh1), EMPTY)->shake (x,h1,h2)
	| (NODE(_,x,lh1,EMPTY), NODE(_,y,lh2,rh2)) -> if x > y then shake (y,merge(lh2,h1),rh2)
												  else shake (x, lh1, h2)
	| (NODE(_,x,lh1,rh1), NODE(_,y,lh2, EMPTY)) -> if x > y then shake (y,lh2,h1)
													else shake (x, lh1, merge(rh1,h2))
	| (NODE(_,x,lh1,rh1), NODE(_,y,lh2, rh2)) -> if x >  y then shake (y,merge(h1, lh2),rh2)
												   else shake (x, lh1, merge(rh1, h2))
let findMin h = 
	match h with 
	| EMPTY -> raise EmptyHeap 
	| NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 
let deleteMin h = 
	match h with 
	| EMPTY -> raise EmptyHeap 
	| NODE(_,x,lh,rh) -> merge (lh,rh)
