type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function 
	EMPTY -> -1
	| NODE(r,_,_,_) -> r

let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh)
	then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)

let rec merge = function (heap1, heap2) -> 
	match heap1, heap2 with
	| (EMPTY, h) -> h
	| (h, EMPTY) -> h
	| ((NODE(_,x1,lh1,rh1)), (NODE(_,x2,lh2,rh2))) -> 
		if x1<x2 
		then shake(x1,lh1,merge(rh1,heap2))
		else shake(x2,lh2,merge(rh2,heap1))

let insert = function (x,h) -> 
	merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin = function 
	EMPTY -> raise EmptyHeap
	| NODE(_,x,_,_) -> x

let deleteMin = function 
	EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge(lh,rh)
