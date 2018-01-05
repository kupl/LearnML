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

let rec merge (a, b) =
	match a with
	| EMPTY -> b
	| NODE(_,x,_,_) -> (match b with
			 | EMPTY -> a
			 | NODE(_,y,_,_) -> if x > y then  shake (y, a, deleteMin b)
						else shake (x, deleteMin a, b))

and findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,_,_) -> x 

and insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

and deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh) 

