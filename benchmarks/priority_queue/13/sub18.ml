(* 2009-11718 1-5*)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h = match h with
	| EMPTY -> -1
	| NODE(r,_,_,_) -> r

let shake (x, lh, rh) =
	if (rank lh) >= (rank rh)
		then NODE(rank rh+1, x, lh, rh)
		else NODE(rank lh+1, x, rh, lh)

let rec merge (lh, rh) =
	let getValue h = 
		match h with
		| EMPTY -> raise EmptyHeap
		| NODE (_,x,_,_) -> x in
	let getLeftHeap h =
		match h with
		| EMPTY -> raise EmptyHeap
		| NODE (_,_,lh,_) -> lh in
	let getRightHeap h =
		match h with
		| EMPTY -> raise EmptyHeap
		| NODE (_,_,_,rh) -> rh in

	match (lh, rh) with
	| (EMPTY, EMPTY) -> raise EmptyHeap
	| (h, EMPTY) -> h
	| (EMPTY, h) -> h
	| (l, r) -> if (getValue l) > (getValue r) then
				 shake ((getValue r), (getLeftHeap r), (merge ((getRightHeap r), l)))
			else shake ((getValue l), (getLeftHeap l), (merge ((getRightHeap l), r)))

let findMin h = match h with
	| EMPTY -> raise EmptyHeap
	| NODE (_,x,_,_) -> x

let insert (x, h) = merge (h, NODE(0,x,EMPTY,EMPTY))

let deleteMin h = match h with
	| EMPTY -> raise EmptyHeap
	| NODE (_,x,lh,rh) -> merge (lh, rh)

