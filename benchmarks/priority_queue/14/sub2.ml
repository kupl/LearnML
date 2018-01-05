(* 2006-11377 hw2-2 *)

type heap = EMPTY | NODE of rank * value * heap * heap 
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1 
| NODE (r, _, _, _) -> r 
				  
let findMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,_,_) -> x

let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh) then 
		NODE(rank rh + 1, x, lh, rh)
	else 
		NODE(rank lh + 1, x, rh, lh)

let rec merge (lh, rh) = 
	match (lh, rh) with
	| (_, EMPTY) -> lh
	| (EMPTY, _) -> rh
	| (NODE(r,x,lChild,rChild),_) ->
		let lMin = findMin lh in
		let rMin = findMin rh in
		if lMin > rMin then
			merge (rh, lh)
		else 
			shake(lMin,lChild,merge(rh,rChild))

let insert = function (x,h) -> 
	merge(h, NODE(0,x,EMPTY,EMPTY))	
	
let deleteMin = function EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge(lh,rh)
	