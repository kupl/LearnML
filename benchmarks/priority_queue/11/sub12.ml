(* 2008-11874 Lee, Sujee *)
(* EXERCISE 4 *)	
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

let rec merge (h1,h2) =
	match (h1,h2) with
		| (h,EMPTY)|(EMPTY,h) -> h
		| (NODE(r1,x1,lh1,rh1),NODE(r2,x2,lh2,rh2)) 
			-> if x1<=x2 then shake(x1,lh1,(merge (rh1,NODE(r2,x2,lh2,rh2))))
			else shake(x2,lh2,(merge (rh2,NODE(r1,x1,lh1,rh1))))
		| (NODE(0,x,EMPTY,EMPTY),NODE(r,xh,lh,rh))|(NODE(r,xh,lh,rh),NODE(0,x,EMPTY,EMPTY)) 
			-> if x<=xh then NODE((r+1),x,NODE(r,xh,lh,rh),EMPTY)
			else shake(xh,lh,(merge (rh,NODE(0,x,EMPTY,EMPTY))))
		| (NODE(0,x1,EMPTY,EMPTY),NODE(0,x2,EMPTY,EMPTY)) 
			-> if x1<=x2 then NODE(0,x1,NODE(0,x2,EMPTY,EMPTY),EMPTY)
			else NODE(0,x2,NODE(0,x1,EMPTY,EMPTY),EMPTY)
	(* O(logn) ? hm 3rd 4th AK! *)

let findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh) 
