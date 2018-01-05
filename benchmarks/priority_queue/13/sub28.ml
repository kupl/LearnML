type rank = int

type value = int

type heap = EMPTY | NODE of rank * value * heap * heap

exception EmptyHeap

let rank = function EMPTY -> -1
		| NODE (r, _, _, _) -> r

let shake = function (x, lh, rh) ->
		if (rank lh) >= (rank rh)
		then NODE (rank rh + 1, x, lh, rh)
		else NODE (rank lh + 1, x, rh, lh)

let findMin = function EMPTY -> raise EmptyHeap
			| NODE (_, x, _, _) -> x

let leftChild = function EMPTY -> raise EmptyHeap
			| NODE (_, _, lh, _) -> lh

let rightChild = function EMPTY -> raise EmptyHeap
			| NODE (_, _, _, rh) -> rh

let rec merge (l, r) =
	match (l, r) with
	| (l, EMPTY) -> l
	| (EMPTY, r) -> r
	| (l, r) -> if (findMin l) < (findMin r) then shake (findMin l, leftChild l, merge (rightChild l, r))
		    else shake (findMin r, leftChild r, merge (rightChild r, l))
	

let insert = function (x, h) -> merge (h, NODE(0, x, EMPTY, EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
			| NODE (_, x, lh, rh) -> merge(lh, rh)
