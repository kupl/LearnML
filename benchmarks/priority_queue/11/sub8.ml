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

let rec merge (h1, h2) = match (h1, h2) with
	(EMPTY, _) -> h2
	| (_, EMPTY) -> h1
	| (NODE(r1,x1,lh1,rh1), NODE(r2,x2,lh2,rh2)) -> 
		if x1>x2 then shake(x2, lh2, merge(h1, rh2))
		else shake(x1, lh1, merge(h2, rh1))
(*
 	if h1 = EMPTY then h2
	else if h2 = EMPTY then h1
	else if h1 = NODE(r1, x1, lh1, rh1) && h2 = NODE(r2, x2, lh2, rh2) then
	(
	 	if x1 > x2 then shake(x2, lh2, merge(h1, rh2)) (* h2가 더 작은 경우, h1이 h2의 자식이 되어야한다.*)
		else shake(x1, lh1, merge(h2, rh1))
	)
*)

let findMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,_,_) -> x

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,lh,rh) -> merge (lh,rh) 

