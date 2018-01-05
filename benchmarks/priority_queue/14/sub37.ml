
type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int


exception EmptyHeap

let rank = function EMPTY -> -1
                  | NODE(r,_,_,_) -> r

let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh)
		then NODE(rank rh + 1, x, lh, rh)
		else NODE(rank lh + 1, x, rh, lh)

let value = function EMPTY -> raise EmptyHeap
		   | NODE(_,v,_,_) -> v

let rec merge ((h1 : heap), (h2 : heap)) : heap =
	if (h1 == EMPTY) then h2
	else if (h2 == EMPTY) then h1
	else if ((value h1) < (value h2)) then begin
		match h1 with
	     	| NODE(0,v,lh,EMPTY) -> shake (v, lh, h2)
	     	| NODE(r,v,lh,rh) -> let a = merge (rh, h2) in shake (v, lh, a)
		| _ -> raise EmptyHeap
	end
	else begin
		match h2 with
		| NODE(0,v,lh,EMPTY) -> shake (v, lh, h1)
		| NODE(r,v,lh,rh) -> let a = merge (rh, h1) in shake (v, lh, a)
		| _ -> raise EmptyHeap
	end

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
                     | NODE(_,x,_,_) -> x

let deleteMin = function EMPTY -> raise EmptyHeap
                       | NODE(_,x,lh,rh) -> merge(lh,rh)
