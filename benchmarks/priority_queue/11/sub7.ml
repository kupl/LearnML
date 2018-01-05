(* 2006-11867 Jo, Dong-Chul *)

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

let rec merge : heap * heap -> heap = fun (lh, rh) ->
	match (lh, rh) with
	| (EMPTY, _) -> rh
	| (_, EMPTY) -> lh
	| (NODE(_,lv,llh,lrh), NODE(_,rv,rlh,rrh)) ->
		if lv > rv then shake(rv, rlh, merge(rrh, lh))
		else shake(lv, llh, merge(lrh, rh))
(* TODO *) 

let findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh) 

