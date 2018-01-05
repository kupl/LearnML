type heap = EMPTY
		| NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1 
                  | NODE (r, _, _, _) -> r

let right_child h =
	match h with
		| EMPTY -> raise EmptyHeap
		| NODE(_,_,lh,rh) -> rh

let left_child h =
	match h with
		| EMPTY -> raise EmptyHeap
		| NODE(_,_,lh,rh) -> lh

let findMin = function EMPTY -> raise EmptyHeap
					| NODE(_,x,_,_ ) -> x

let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh) then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)


let rec merge (h1, h2) =
	if (h1 = EMPTY) then h2
	else if (h2 = EMPTY) then h1
	else if (findMin h1) > (findMin h2) then merge(h2, h1)
	else shake (findMin h1, left_child h1, merge(right_child h1, h2))

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
					| NODE(_,x,lh,rh) -> merge(lh,rh)



	