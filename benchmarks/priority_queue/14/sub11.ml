type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int
;;
let rank = function EMPTY -> -1
	| NODE(r,_,_,_) -> r
;;

exception EmptyHeap ;;
let findMin = function EMPTY -> raise EmptyHeap
		| NODE (_,x,_,_) -> x
;;
let rec merge (one, another) = 
	let shake = function (x, lh, rh) ->
		if (rank lh) >= (rank rh)
			then NODE(rank rh + 1, x, lh, rh)
			else NODE(rank lh + 1, x, rh, lh)
	in
	if one = EMPTY then another
	else if another = EMPTY then one
	else if (findMin one) <= (findMin another) then
		match one with
		| NODE (r, v, lh, rh) -> shake(v, lh, merge(rh, another))
		| EMPTY -> another
	else
		match another with
		| NODE (r, v, lh, rh) -> shake(v, lh, merge(rh, one))
		| EMPTY -> one
;;
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
;;
let deleteMin = function EMPTY -> raise EmptyHeap
		| NODE(_,x,lh,rh) -> merge(lh,rh)
;;
