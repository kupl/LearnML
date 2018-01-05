exception EmptyHeap

type rank = int
and value = int
and heap =
	| EMPTY
	| NODE of rank * value * heap * heap

let rank = function 
	EMPTY -> -1
	| NODE (r, _, _, _) -> r

let findMin = function 
	EMPTY -> raise EmptyHeap
	| NODE (_, v, _, _) -> v

let shake = function (x, lh, rh) ->
	if (rank lh) >= (rank rh)
	then NODE((rank rh) + 1, x, lh, rh)
	else NODE((rank lh) + 1, x, rh, lh)

let childs = function
	EMPTY -> raise EmptyHeap
	| NODE (_, _, lh, rh) -> (lh, rh)

let rec merge ((l: heap), (r:heap)): heap =
	match (l, r) with
		| (EMPTY, EMPTY) -> EMPTY
		| (lh, EMPTY) -> lh
		| (EMPTY, rh) -> rh
		| (lh, rh) -> (
			let small, large =
				if (findMin lh) < (findMin rh)
				then lh, rh
				else rh, lh in
				shake ((findMin small), merge (childs small), large)
			)

let deleteMin = function 
	EMPTY -> raise EmptyHeap
	| NODE (_, x, lh, rh) -> merge(lh, rh)

let insert = function (x, h) ->
	merge(h, NODE(0, x, EMPTY, EMPTY))