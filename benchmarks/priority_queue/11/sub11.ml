
(* 2008-11720 Á¶°Ü¸® *)

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
	match h1 with
	EMPTY -> h2
	| NODE (r, v, lh, rh) -> 
		match h2 with
		EMPTY -> h1
		| NODE (r2, v2, lh2, rh2)-> 
			if (findMin h1)<(findMin h2) then shake (v, lh, (merge (rh, h2)))
			else shake (v2, lh2, (merge (rh2, h1)))

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin h = match h with
    | EMPTY -> raise EmptyHeap
    | NODE(_,x,lh,rh) -> merge (lh,rh)

