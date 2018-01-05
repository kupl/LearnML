type heap =
	| EMPTY
	| NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h =
	match h with
	| EMPTY -> 0
	| NODE (r, _, _, _) -> r

let shake = fun (x, lh, rh) ->
	if (rank lh) >= (rank rh)
	then NODE (rank rh + 1, x, lh, rh)
	else NODE (rank lh + 1, x, rh, lh)

let rec merge (h1, h2) =
	match (h1, h2) with
	| (EMPTY, h2) -> h2
	| (h1, EMPTY) -> h1
	| (NODE (r1, v1, lh1, rh1), NODE (r2, v2, lh2, rh2)) ->
		if v1 > v2 then shake (v2, lh2, merge (h1, rh2))
		else shake (v1, lh1, merge (rh1, h2))

let insert (x, h) = merge (h, NODE (0, x, EMPTY, EMPTY))

let findMin h =
	match h with
	| EMPTY -> raise EmptyHeap
	| NODE (_, x, _, _) -> x

let deleteMin h =
	match h with
	| EMPTY -> raise EmptyHeap
	| NODE (_, x_, lh, rh) -> merge (lh, rh)