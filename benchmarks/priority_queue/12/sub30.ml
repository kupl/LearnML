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

let findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,_,_) -> x 

let rec merge (h1, h2) = 
	match (h1, h2) with
	| (h1, EMPTY) -> h1
	| (EMPTY, h2) -> EMPTY
        | (NODE(a1,b1,c1,d1), NODE(a2,b2,c2,d2)) -> if b1>b2 then 
		shake(b2,c2,merge(h1,d2))
		else shake(b1,c1,merge(d1,h2))
(* b1>b2 일때 b2, c2를 h1그대로와 rh2를 merge해서 쉐이킹 *)

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh) 




		
