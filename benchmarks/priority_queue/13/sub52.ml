type heap = EMPTY | NODE of rank * value * heap * heap
			and rank = int
			and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
			| NODE(r,_,_,_) -> r


let shake = function (x,lh,rh) -> if (rank lh) >= (rank rh)
				then NODE(rank rh + 1, x, lh, rh)
				else NODE(rank lh + 1, x, rh, lh)

let rec merge (lh, rh) =
	match (lh, rh) with
	| (EMPTY, rh) -> rh
	| (lh, EMPTY) -> lh
	| (NODE(_,x,ll,lr),NODE(_,y,rl,rr)) -> if x <= y then shake(x, ll, merge(lr,rh))
					else shake(y,rl,merge(lh,rr))

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
			| NODE(_, x, lh, rh) -> merge(lh, rh)

let findMin = function EMPTY -> raise EmptyHeap
				| NODE(_,x,_,_) -> x