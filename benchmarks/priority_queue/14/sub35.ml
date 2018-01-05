type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1 
                  | NODE (r,_,_,_) -> r 

let findMin = function EMPTY -> raise EmptyHeap
					 | NODE (_,x,_,_) -> x
		
let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh)
		then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)

let rec merge = function (h1, h2) ->
	match (h1, h2) with
	| (EMPTY, h2) -> h2
	| (h1, EMPTY) -> h1
	| (NODE (r1,v1,lh1,rh1) , NODE (r2,v2,lh2,rh2)) 
		-> if v1 < v2 then shake (v1, lh1, merge (rh1, h2))
		   else shake (v2, lh2, merge (rh2, h1))

(*
let rec merge_rec = function (h1, h2) ->
	match (h1, h2) with
	| (EMPTY, h2) -> h2
	| (h1, EMPTY) -> h1
	| (NODE (r1,v1,lh1,rh1) , NODE (r2,v2,lh2,rh2)) 
		-> if v1 < v2 then NODE (r1, v1, lh1, merge_rec (rh1, h2))
		   else NODE(r2, v2, lh2, merge_rec (rh2, h1))


let merge = function (h1, h2) ->
	match (merge_rec (h1, h2)) with
	| EMPTY -> EMPTY
	| NODE (r, v, lh, rh) -> shake (v, lh, rh)
*)

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
			
let deleteMin = function EMPTY -> raise EmptyHeap
					   | NODE(_,x,lh,rh) -> merge(lh,rh)


		
