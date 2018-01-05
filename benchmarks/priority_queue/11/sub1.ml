(* 2004-11951 Noh, Soon Hyun *)

(* type predefined *)
type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h = match h with
	| EMPTY -> -1
	| NODE(r, _, _, _) -> r

(* shake: value * heqp * heap -> heqp *)
let shake (x, lh, rh) =
	if (rank lh) >= (rank rh) then
		NODE (rank rh+1, x, lh, rh)
	else
		NODE (rank lh+1, x, rh, lh)

(* ToDo *)
(* merge: heap * heap -> heap *)
let rec merge (h1, h2) =
	match (h1, h2) with
	| (EMPTY, h) -> h
	| (h, EMPTY) -> h
	| (NODE (r1, v1 ,lh1, rh1), NODE (r2, v2, lh2, rh2))
		-> if v1 < v2 then
				shake (v1, lh1,
						   merge (rh1, NODE (r2, v2, lh2, rh2)))
		   else
		   		shake (v2, lh2,
						   merge (rh2, NODE (r1, v1, lh1, rh1)))

let findMin h = match h with
	| EMPTY -> raise EmptyHeap
	| NODE (_, x, _, _) -> x

let insert(x, h) = merge(h, NODE(0, x, EMPTY, EMPTY))

let deleteMin h = match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_, x, lh, rh) -> (merge (lh, rh))

(* Test Case 
let print2nd h = match h with
	| NODE(_, v1, NODE(_, v2, _, _), _)
		-> print_int v1; print_int v2
	| _ -> print_char 'n'
let two = insert(2, EMPTY)
let one_two = insert(1, two)
let one_to_four = insert (3,
						  insert (2,
								  insert (1,
								  		  insert (2, EMPTY))))

let _ = print_int (findMin (deleteMin one_two))
let _ = print_int (findMin one_to_four)
let _ = print_int (findMin (deleteMin one_to_four))
let _ = print_int (findMin (deleteMin (deleteMin one_to_four)))
let _ = print_int (findMin (deleteMin (deleteMin (deleteMin one_to_four))))
*)
