type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function EMPTY -> -1
					| NODE(r,_,_,_) -> r
let findMin = function EMPTY -> raise EmptyHeap
						| NODE(_,x,_,_) -> x
	
let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh)
	then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)
	
let rec merge (heap1,heap2) =
	match (heap1,heap2) with
	| (EMPTY,h) -> h
	| (h,EMPTY) -> h
	| (NODE(r1,x1,h1_l,h1_r),NODE(r2,x2,h2_l,h2_r)) ->
		if x1 <= x2
		then shake(x1,h1_l,merge(h1_r,heap2))
		else shake(x2,h2_l,merge(h2_r,heap1))
		
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
let deleteMin = function EMPTY -> raise EmptyHeap
						| NODE(_,x,lh,rh) -> merge(lh,rh)
