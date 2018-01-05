(* hw2-3 *)
(* 2010-11687 Keunjun Choi *)


type rank = int
and value = int
and heap = EMPTY
    | NODE of rank * value * heap * heap

exception EmptyHeap 

let rank h =
    match h with 
    | EMPTY -> -1 
    | NODE(r,_,_,_) -> r 

let shake (x,lh,rh) = 
    if (rank lh) >= (rank rh) 
	then NODE(rank rh+1, x, lh, rh) 
    else NODE(rank lh+1, x, rh, lh) 

let rec merge (lh, rh) =
    let dM h =
    match h with
    | EMPTY -> raise EmptyHeap
    | NODE(_,x,lh,rh) -> merge (lh, rh)
    in

    match (lh, rh) with
    | (EMPTY, r) -> r
    | (l, EMPTY) -> l
    | (NODE (lr, lx, llh, lrh), NODE (rr, rx, rlh, rrh)) ->
	(if (rx < lx) then
	    (shake (rx, lh, (dM rh)))
	else (shake (lx, (dM lh), rh)))
		  
let findMin h = 
    match h with 
    | EMPTY -> raise EmptyHeap 
    | NODE(_,x,_,_) -> x 
		      
let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h =
    match h with
    | EMPTY -> raise EmptyHeap
    | NODE(_,x,lh,rh) -> merge (lh,rh)
