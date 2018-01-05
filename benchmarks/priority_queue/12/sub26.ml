type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap


let 	rank x =
	match x with
	|	EMPTY -> -1
	|	NODE(r,_,_,_) -> r

let shake (x, lh, rh) = if (rank lh) >= (rank rh)
			then NODE(rank rh + 1, x, lh, rh)
			else NODE(rank lh + 1, x, rh, lh)


let rec merge (hp1, hp2) =
	match hp1, hp2 with
	|	NODE(rnk1, v1, lh1, rh1), 
		NODE(rnk2, v2, lh2, rh2) -> (if (v1 > v2) 
									then (let res = merge(hp1, rh2) in shake(v2, lh2 , res))
									else (let res = merge(hp2, rh1) in shake( v1, lh1, res)))
	|	hp, EMPTY -> hp
	|	EMPTY, hp -> hp




let  insert(x, h) = merge(h, NODE(0, x, EMPTY, EMPTY))

let 	findMin x =
	match x with
	| EMPTY -> raise EmptyHeap
	| NODE(_,x,_,_) -> x

let deleteMin x =
	match x with
	|	EMPTY -> raise EmptyHeap
	| 	NODE(_, x, lh, rh) -> merge(lh, rh)


	
