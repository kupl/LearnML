
(* ex 4 *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h = match h with
	| EMPTY -> -1
	| NODE(r,_,_,_) -> r

let findMin h = match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_,x,_,_) -> x

let shake(x,lh,rh) = if (rank lh) >= (rank rh)
					 then NODE(rank rh+1,x,lh,rh)
					 else NODE(rank lh+1,x,rh,lh)

let rec merge(heap1,heap2) = 
	match(heap1,heap2) with
	| (EMPTY,EMPTY) -> EMPTY
	| (EMPTY,rh) -> rh
	| (lh,EMPTY) -> lh
	| (NODE(lh_r,lh_x,lh_lh,lh_rh),NODE(rh_r,rh_x,rh_lh,rh_rh)) -> 
		if (lh_x <= rh_x) then 
			shake(lh_x,lh_lh,merge(lh_rh,NODE(rh_r,rh_x,rh_lh,rh_rh)))
		else 		
			merge(NODE(rh_r,rh_x,rh_lh,rh_rh),NODE(lh_r,lh_x,lh_lh,lh_rh))

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin h = match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge(lh,rh)
