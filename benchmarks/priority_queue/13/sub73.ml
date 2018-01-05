(* file name : ex5.ml *)
(* author : Jisoon Park (jspark@ropas.snu.ac.kr) *)
(* date : 2013-09-13 *)
(* Exercise 5 *)
type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function EMPTY -> -1
	| NODE(r, _, _, _) -> r

let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh)
		then NODE(rank rh + 1, x, lh, rh)
		else NODE(rank lh + 1, x, rh, lh)

let rec merge : heap * heap -> heap
 = fun (h1, h2) ->
	match (h1, h2) with
	| (_, EMPTY) -> h1
	| (EMPTY, _) -> h2 
	| (NODE(_, lv, llh, lrh), NODE(_, rv, rlh, rrh)) -> 
		if (lv > rv)
			then shake(rv, rlh, merge(h1, rrh))
			else shake(lv, llh, merge(h2, lrh)) 


let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
let findMin = function EMPTY -> raise EmptyHeap
	| NODE(_,x, _, _) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
	| NODE(_,x,lh,rh) -> merge(lh,rh)