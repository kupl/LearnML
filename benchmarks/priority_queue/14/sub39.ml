exception TODO (*done*)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int
exception EmptyHeap

let rank = function EMPTY -> -1 
	| NODE (r, _, _, _) -> r

let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh)
	then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)
	
let rec merge ((h1:heap), (h2:heap)): heap = 
	match h1, h2 with
	| EMPTY, _ -> h2
	| _, EMPTY -> h1
	| NODE (_, x, sh1l, sh1r), NODE (_ , y, sh2l, sh2r) ->
		if x <= y then shake (x, sh1l, merge (sh1r, h2))
		else shake (y, sh2l, merge (h1, sh2r))

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
	| NODE(_,x,_,_) -> x

let deleteMin = function EMPTY -> raise EmptyHeap		
	| NODE(_,x,lh,rh) -> merge(lh,rh)


	


		
	
