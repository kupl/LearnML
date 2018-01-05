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

let rec merge (h1, h2) =
	match h1 with
	EMPTY -> h2
	| NODE(_,x1,lh1,rh1) -> 
		(match h2 with
		 EMPTY -> h1
		 | NODE(_,x2,lh2,rh2) -> 
		 	(if x1 < x2 then merge(rh1, shake(x1, lh1, h2))
			 else merge(rh2, shake(x2, lh2, h1)))
		)

let findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh) 
