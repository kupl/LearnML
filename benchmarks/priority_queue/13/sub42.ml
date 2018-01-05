type heap = EMPTY | NODE of rank * value * heap * heap
	and rank = int
	and value = int

exception EmptyHeap
let rank = function EMPTY -> -1
									| NODE(r,_,_,_) -> r

let shake = function (x, lh, rh) ->
	if (rank lh) >= (rank rh)
		then NODE(rank rh + 1, x, lh, rh)
		else NODE(rank lh + 1, x, rh, lh)

let rec merge = fun (h1, h2) ->
	match (h1, h2) with
		(EMPTY, a) -> a
		| (a, EMPTY) -> a
		| (NODE(r1, v1, lc1, rc1), NODE(r2, v2, lc2, rc2)) -> 
				if v1 > v2
					then (merge (NODE(r2, v2, lc2, rc2), NODE(r1, v1, lc1, rc1)))
					else shake (v1, lc1, merge(rc1, NODE(r2, v2, lc2, rc2)))

let insert = function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))
let findMin = function EMPTY -> raise EmptyHeap
											| NODE(_,x,_,_) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
												| NODE(_,x,lh,rh) -> merge(lh, rh)

