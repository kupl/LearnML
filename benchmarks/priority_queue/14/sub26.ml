(* College of Liberal Studies 2010-13342 Kim Ye Jung *)
(* 2014.2 Programming Languages Homework 2 - 2 *)
type heap = EMPTY
	| NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
	| NODE(r,_,_,_) -> r
	
let shake = function(x, lh, rh) ->
	if (rank lh) >= (rank rh)
	then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)
	
let rec merge = function(EMPTY, x) -> x
	| (x, EMPTY) -> x
	| (NODE(rank1, val1, lh1, rh1), NODE(rank2, val2, lh2, rh2)) ->
		if val1 < val2
		then shake(val1, lh1, merge(rh1, NODE(rank2, val2, lh2, rh2)))
		else shake(val2, lh2, merge(rh2, NODE(rank1, val1, lh1, rh1)))

let insert = function(x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
	| NODE(_,x,_,_) -> x
	
let deleteMin = function EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge(lh, rh)
	
