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
	      then NODE(rank rh+1,x,lh,rh)
	    else NODE(rank lh+1,x,rh,lh)

let rec merge (h1,h2) =
	match (h1,h2) with
	|(_,EMPTY) -> h1
	|(EMPTY,_) -> h2
	|(NODE(_,x,a1,b1),NODE(_,y,a2,b2)) -> 
		if x <= y then shake(x, a1, merge(b1,h2))
		else shake(y, a2, merge(h1,b2))

let insert = function (x,h) -> merge(h, NODE(-1,x,EMPTY,EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
              | NODE(_,x,lh,rh) -> merge(lh,rh)

