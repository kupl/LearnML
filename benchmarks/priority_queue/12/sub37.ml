type heap = EMPTY | NODE of rank * value * heap * heap 
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

let rec merge (hp1,hp2) = 
	match (hp1,hp2) with
	|(EMPTY,hp2) -> hp2
	|(hp1,EMPTY) -> hp1
	|(_,_) -> if ((findMin hp1)>=(findMin hp2)) then shake(findMin hp2, deleteMin hp2, hp1)
		  else shake(findMin hp1, deleteMin hp1, hp2)

and findMin h = 
	match h with 
	| EMPTY -> raise EmptyHeap 
	| NODE(_,x,_,_) -> x 

and insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 
and deleteMin h = 
	match h with 
	| EMPTY -> raise EmptyHeap 
	| NODE(_,x,lh,rh) -> merge (lh,rh) 

