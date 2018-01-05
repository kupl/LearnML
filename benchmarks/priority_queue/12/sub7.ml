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
	|(EMPTY,EMPTY) -> EMPTY
 	|(EMPTY,rh) -> rh
	|(lh,EMPTY) -> lh
	|(NODE(rank,vl,lhl,rhl),NODE(_,vr,_,_)) -> 
				if vl > vr then merge(rh,lh)
				else if rank == 0 then shake(vl,lhl,rh)
				else shake(vl,lhl,merge(rhl,rh))

let findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh)

