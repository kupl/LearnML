type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int
exception EmptyHeap
let rank = function EMPTY -> -1
	| NODE(r,_,_,_) -> r
let leftheap = function EMPTY -> raise EmptyHeap
	| NODE(_,_,l,_) -> l
let rightheap = function EMPTY -> raise EmptyHeap
	| NODE(_,_,_,r) -> r
let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh) then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)
let findMin = function EMPTY -> raise EmptyHeap
	| NODE(_,x,_,_) -> x

let rec merge(h1,h2) =
	if (rank h1) == -1 then h2
	else if (rank h2) == -1 then h1
	else begin
		if (findMin h1) > (findMin h2) then merge(h2,h1)
		else shake(findMin(h1), leftheap(h1), merge(rightheap(h1),h2))
	end
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge(lh,rh)

