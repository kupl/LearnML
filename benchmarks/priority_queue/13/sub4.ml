type heap = EMPTY
	| NODE of rank * value * heap * heap
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
let rec merge (h1,h2) = 
	match (h1,h2) with
	| (EMPTY,EMPTY) -> EMPTY
	| (h1,EMPTY) -> h1
	| (EMPTY,h2) -> h2
	| (NODE(r1,x1,h11,h12),NODE(r2,x2,h21,h22)) ->
		if x1 >= x2 then
			shake (x2,h1,merge(h21,h22))
		else 
			shake (x1,merge(h11,h12),h2)
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
let deleteMin = function EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge(lh,rh)

