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

let rec merge (heap1,heap2) =
	match (heap1,heap2) with
	| (EMPTY,_) -> heap2
	| (_,EMPTY) -> heap1
	| (NODE(r1,x1,lh1,rh1),NODE(r2,x2,lh2,rh2)) ->
			if x1 <= x2
			then shake (x1,lh1,merge(rh1, heap2))
			else shake (x2,lh2,merge(rh2, heap1))

let findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh) 
