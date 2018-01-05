(* HW2 Exercise3 2009-11697 Kim HyunJoon *)
(* Priority Queue *)



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

let rec merge : heap * heap -> heap =
	fun (h1, h2) ->
	let left h = match h with
		| EMPTY -> raise EmptyHeap
		| NODE(_,_,l,_) -> l
	in
	let right h = match h with
		| EMPTY -> raise EmptyHeap
		| NODE(_,_,_,r) -> r
	in
	match (h1, h2) with
	| (_, EMPTY) -> h1
	| (EMPTY, _) -> h2
	| (_, _) -> 
		if (findMin h1) <= (findMin h2) 
		then shake (findMin h1, left h1, merge ((right h1), h2))
		else shake (findMin h2, left h2, merge ((right h2), h1))

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh)

(*
let five = NODE(0, 5, EMPTY, EMPTY)
let three = NODE(0, 3, EMPTY, EMPTY)

*)
