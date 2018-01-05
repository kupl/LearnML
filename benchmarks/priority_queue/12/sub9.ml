exception EmptyHeap

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

let rank ex =
	match ex with
	| EMPTY -> 0
	| NODE(r,_,_,_) -> r
let findMin ex =
	match ex with
	| EMPTY -> raise EmptyHeap
	| NODE (_,x,_,_) -> x
let shake (x,lh,rh) = if (rank lh) >= (rank rh)
		      then NODE(rank rh+1, x, lh, rh)
		      else NODE(rank lh+1, x, rh, lh)
let findLeftHeap x =
	match x with
	| EMPTY -> raise EmptyHeap
	| NODE(_,_,lh,_) -> lh
let findRightHeap x =
	match x with
	| EMPTY -> raise EmptyHeap
	| NODE(_,_,_,rh) -> rh
let rec merge (lh, rh) =
	match (lh, rh) with
	| (EMPTY, EMPTY) -> raise EmptyHeap
	| (lh, EMPTY) -> lh
	| (EMPTY, rh) -> rh
	| (lh, rh) -> if findMin lh > findMin rh then shake (findMin rh, findLeftHeap rh, merge(findRightHeap rh, lh))
		      else shake (findMin lh, findLeftHeap lh, merge(findRightHeap lh, rh))

let insert (x, h) = merge(h, NODE(0, x, EMPTY, EMPTY))
let deleteMin ex =
	match ex with
	| EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge(lh, rh)
