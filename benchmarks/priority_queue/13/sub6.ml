exception EmptyHeap

type heap = EMPTY | NODE of rank*value*heap*heap
and rank = int
and value = int

let rank = function EMPTY -> -1 
					| NODE(r,_,_,_) -> r

let findMin = function EMPTY -> raise EmptyHeap
					| NODE(_,x,_,_) -> x

let findMin h= match h with
	| EMPTY -> raise EmptyHeap
	| NODE(_,x,_,_) -> x


let shake (x, lh, rh) = function (x, lh, rh) ->
							if(rank lh) >= (rank rh)
							then NODE(rank rh+1, x, lh, rh)
							else NODE(rank lh+1, x, rh, lh)

let shake (x, lh, rh) = 
							if(rank lh) >= (rank rh)
							then NODE(rank rh+1, x, lh, rh)
							else NODE(rank lh+1, x, rh, lh)

let getLh=function EMPTY -> EMPTY
					| NODE(_,_,x,_) -> x

let getRh=function EMPTY -> EMPTY
					| NODE(_,_,_,x) -> x


let rec merge (h1,h2)=
	 match (h1,h2) with   		
		|(_, EMPTY) -> h1
		|(EMPTY, _) -> h2 
		|(_,_) -> if (findMin h1) <= (findMin h2)
					then shake (findMin h1, getLh h1, merge(getLh h1, h2))
					else shake (findMin h2, getRh h2, merge(h1, getRh h2))


let rec merge (h1,h2)=
	 match (h1,h2) with   		
		|(_, EMPTY) -> h1
		|(EMPTY, _) -> h2 
		|(_,_) -> if (findMin h1) <= (findMin h2)
					then shake (findMin h1, getLh h1, merge(getRh h1, h2))
					else shake (findMin h2, getLh h2, merge(h1, getRh h2))


let insert = function (x,h) -> merge(h, NODE(0, x, EMPTY, EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
					| NODE(_,x,lh,rh) -> merge(lh,rh)




	
	
							
(*	
let t2 = insert (1, insert (10, insert(30,EMPTY)))  
let t3 = insert (25, insert (34, insert(9, insert (11,EMPTY)))) 
let t5 = merge (t2,t3)  
let _ = (findMin t5,findMin (deleteMin t5)) (* 1,9 *)

*)
