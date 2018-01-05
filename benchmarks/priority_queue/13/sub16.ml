type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank =
	function EMPTY -> -1
	| NODE(r, _, _, _) -> r

let findMin =
	function EMPTY -> raise EmptyHeap
	| NODE(_, x, _, _) -> x

let shake =
	function (x, lh, rh) ->
		if (rank lh) >= (rank rh) then NODE(rank rh+1, x, lh, rh)
		else NODE(rank lh+1, x, rh, lh)

let rec merge (h1, h2) = 
	match (h1, h2) with
	| (EMPTY, EMPTY) -> EMPTY
	| (EMPTY, x) -> x
	| (x, EMPTY) -> x
	|_ ->
		if (findMin h1 < findMin h2) then my_merge(h1, h2) 
		else my_merge(h2, h1)

and my_merge (h1, h2) =
	match h1 with
	|EMPTY -> raise EmptyHeap
	|NODE(rank, value, lh, rh) ->
		if lh=EMPTY then NODE(rank, value, h2, rh)
		else shake(value, lh, merge(rh, h2))

and insert =
	function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))

and deleteMin =
	function EMPTY -> raise EmptyHeap
	| NODE(_, x, lh, rh) -> merge(lh, rh)


