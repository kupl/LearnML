type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h = match h with
	| EMPTY -> -1
	| NODE(r, _, _, _) -> r

let shake (x, lh, rh) =
	if (rank lh) >= (rank rh)
	then NODE(rank rh +1, x, lh, rh)
	else NODE(rank lh +1, x, rh, lh)

let findMin h = match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_, x, _, _) -> x

let rec merge = (fun (a, b) -> match (a, b) with
				| (EMPTY, b) -> b
				| (a, EMPTY) -> a
				| (NODE (r1, v1, al, ar), NODE (r2, v2, bl, br)) ->
						if v1 <= v2 then shake (v1, al, (merge (ar, b)))
						else shake (v2, bl, (merge (a, br)))
	)

let insert(x, h) = merge(h, NODE(0, x, EMPTY, EMPTY))

let deleteMin h = match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_, x, lh, rh) -> merge (lh, rh)