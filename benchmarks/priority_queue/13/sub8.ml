type heap =
	EMPTY
	| NODE of rank * value * heap * heap
	and rank = int
	and value = int
exception EmptyHeap
let rank = function
		EMPTY -> -1
		| NODE(r, _, _, _) -> r
let findMin = function
		EMPTY -> raise EmptyHeap
		| NODE(_, x, _, _) -> x
let shake = function (x,lh,rh) ->
		if (rank lh) >= (rank rh)
		then NODE(rank rh + 1, x, lh, rh)
		else NODE(rank lh + 1, x, rh, lh)
let rec merge = function (lh, rh) ->
		match (lh, rh) with
		| (EMPTY, EMPTY) -> EMPTY
		| (EMPTY, rh) -> rh
		| (lh, EMPTY) -> lh
		| (NODE(r1, x1, lh1, rh1), NODE(r2, x2, lh2, rh2)) ->
			if x1 < x2
			then shake(x1, lh1, merge(rh1, NODE(r2, x2, lh2, rh2)))
			else shake(x2, merge(NODE(r1, x1, lh1, rh1), rh2), rh1)
let insert = function (x, h)
		-> merge(h, NODE(0, x, EMPTY, EMPTY))
let deleteMin = function
		EMPTY -> raise EmptyHeap
		| NODE(_ ,x, lh, rh) -> merge(lh, rh)