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
	match (h1, h2) with
	| (EMPTY, h) -> h
	| (h, EMPTY) -> h
	| (NODE (r1, v1, h11, h12), NODE (r2, v2, h21, h22)) -> if (v1 < v2) then (shake (v1, (merge (h11, h2)), h12))
								else (shake (v2, (merge (h21, h1)), h22))
	






let findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh)
