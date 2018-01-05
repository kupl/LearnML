(* merge: heap * heap -> heap *)

type heap = EMPTY
	  | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank h = match h with
EMPTY -> 0
| NODE (r, _, _, _) -> r

let shake (x, lh, rh) = if (rank lh) >= (rank rh)
			then NODE (rank rh + 1, x, lh, rh)
			else NODE (rank lh + 1, x, rh, lh)

let rec merge (lh, rh) = match (lh, rh) with
(EMPTY, _) -> rh
| (_, EMPTY) -> lh
| (NODE (_, lv, EMPTY, EMPTY), NODE (_, rv, EMPTY, EMPTY)) ->
	if lv < rv then NODE (0, lv, NODE (0, rv, EMPTY, EMPTY),
				     EMPTY)
		   else NODE (0, rv, NODE (0, lv, EMPTY, EMPTY),
			   	     EMPTY)
| (NODE (_, lv, lh0, lh1), NODE (_, rv, rh0, rh1)) ->
	if lv < rv then shake (lv,
			       lh0,
			       merge (lh1, rh))
		   else shake (rv,
			       rh0,
			       merge (rh1, lh))

let insert (x, h) = merge (h, NODE(0, x, EMPTY, EMPTY))
let findMin h = match h with
EMPTY -> raise EmptyHeap
| NODE (_, x, _, _) -> x
let deleteMin h = match h with
EMPTY -> raise EmptyHeap
| NODE (_, x, lh, rh) -> merge (lh, rh)

(* Examples *)
let l = NODE(1, 5, NODE(0, 10, EMPTY, EMPTY),
		   NODE(0, 12, EMPTY, EMPTY));;
let r = NODE(1, 3, NODE(0, 7, NODE(0, 14, EMPTY, EMPTY),
			      EMPTY),
		   NODE(0, 8, EMPTY, EMPTY));;
let l0 = NODE(1, 2, NODE(0, 8, NODE(0, 16, EMPTY, EMPTY),
			       EMPTY),
		    NODE(0, 12, EMPTY, EMPTY));;
let r0 = NODE(1, 1, NODE(0, 9, NODE(0, 20, EMPTY, EMPTY),
				EMPTY),
		    NODE(0, 7, EMPTY, EMPTY));;
