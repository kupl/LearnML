type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1 | NODE(r,_,_,_) -> r

let getleft= function EMPTY -> EMPTY | NODE(_,_,lh,_) -> lh

let getright= function EMPTY -> EMPTY | NODE(_,_,_,rh) -> rh

let findMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,_,_) -> x

let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh)
	then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)

let rec merge = function (EMPTY, EMPTY) -> EMPTY
		| (EMPTY, rh) -> rh
		| (lh, EMPTY) -> lh
		| (lh, rh) ->
			if((findMin lh)<(findMin rh)) then shake(findMin lh, getleft lh, merge(getright lh, rh))
			else shake(findMin rh, getleft rh, merge(getright rh, lh))

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,lh,rh) -> merge(lh,rh)

