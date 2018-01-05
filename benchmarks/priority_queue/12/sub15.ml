(* ex3 *)
type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h = match h with
	  EMPTY -> -1
	| NODE (r, _, _, _) -> r

let shake (x, lh, rh) = 
	if (rank lh) >= (rank rh)
	then NODE (rank rh + 1, x, lh, rh)
	else NODE (rank lh + 1, x, rh, lh)

let findMin h = match h with
	  EMPTY -> raise EmptyHeap
	| NODE (_, x, _, _) -> x

let rec merge (lh, rh) = 
	let getLeft h = match h with
		  EMPTY -> raise EmptyHeap
		| NODE (_, _, l, _) -> l in
	
	let getRight h = match h with
		  EMPTY -> raise EmptyHeap
		| NODE (_, _, _, r) -> r in

	match (lh, rh) with
	  (EMPTY, _) -> rh
	| (_, EMPTY) -> lh
	| _ -> if (findMin lh) > (findMin rh) then merge (rh, lh)
		   else 
		        let r = merge (getRight lh, rh) in
				shake (findMin lh, r, getLeft lh)

let insert (x, h) = merge (h, NODE (0, x, EMPTY, EMPTY))

let deleteMin h = match h with
	  EMPTY -> raise EmptyHeap
	| NODE (_, x, lh, rh) -> merge (lh, rh)