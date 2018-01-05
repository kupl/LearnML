(* KIHWAN KANG HW01-5 *)

(* PREDEFINED TYPES *)
type heap =
	EMPTY
	|NODE of rank * value * heap * heap
and rank = int
and value = int
(* END OF PREDEFINED TYPES *)

(* PREDEFINED EXCEPTION *)
exception EmptyHeap
(* END OF PREDEFINED EXCEPTION *)

(* PREDEFINED INDEPENDENT FUNCTIONS *)
let rank = function
	EMPTY -> -1
	|NODE(r,_,_,_) -> r

let findMin = function
	EMPTY -> raise EmptyHeap
	|NODE(_,x,_,_) -> x
(* END OF PREDEFINED INDEPENDENT FUNCTIONS *)

(* MERGE *)
let rec merge (bheap, iheap) =
	(* PREDEFINED shake *)
	let shake = function (x, lh, rh) ->
		if (rank lh) >= (rank rh)
		then NODE(rank rh + 1, x, lh, rh)
		else NODE(rank lh + 1, x, rh, lh)
	(* END OF SHAKE *)
in
	match (bheap, iheap) with
	|(EMPTY, _) -> iheap
	|(_, EMPTY) -> bheap
	|(NODE (_, bh_x, bh_l, bh_r), NODE (_, ih_x, ih_l, ih_r)) ->
		if bh_x < ih_x
		then 
		merge ( 
			(if (rank bh_l) = (rank bh_r) 
			then shake (bh_x, merge (bh_l, NODE (0, ih_x, EMPTY, EMPTY)), bh_r)
			else shake (bh_x, bh_l, merge (bh_r, NODE (0, ih_x, EMPTY, EMPTY)))
			) 
		, merge (ih_l, ih_r))
		else 
		merge ( 
			(if (rank bh_l) = (rank bh_r)
			then shake (ih_x, merge (bh_l, NODE (0, bh_x, EMPTY, EMPTY)), bh_r)
			else shake (ih_x, bh_l, merge (bh_r, NODE (0, bh_x, EMPTY, EMPTY)))
			) 
		, merge (ih_l, ih_r))
(* END OF MERGE *)

(* PREDEFINED MERGE-DEPENDENT FUNCTIONS *)
let insert = function 
	(x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))

let deleteMin = function
	EMPTY -> raise EmptyHeap
	|NODE(_,x,lh,rh) -> merge(lh,rh)
(* END OF MERGE-DEPENDENT FUNCTIONS *)
