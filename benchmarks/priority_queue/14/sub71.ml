type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int;;

exception EmptyHeap;;

let rank = function EMPTY -> -1 | NODE(r,_,_,_) -> r;;
let value = function EMPTY -> raise EmptyHeap | NODE(_,x,_,_) -> x;;
let shake = function (x, lh, rh) ->
	if (rank lh) >= (rank rh)
		then NODE(rank rh + 1, x, lh, rh)
		else NODE(rank lh + 1, x, rh, lh);;
let rec merge = function
	| (EMPTY, EMPTY) -> EMPTY
	| (lh, EMPTY) ->
		begin match lh with
		| NODE(_,x,lhl,lhr) -> shake(x,merge(lhl,lhr),EMPTY) 
		end
	| (EMPTY, rh) ->
		begin match rh with
		| NODE(_,x,rhl,rhr) -> shake(x,merge(rhl,rhr),EMPTY)
		end
	| (lh, rh) ->
		let sm = if (value rh) >= (value lh) then lh else rh in
		let lg = if (value rh) >= (value lh) then rh else lh in
		begin match sm with
		| NODE(_,x,sml,smr) -> shake(x,merge(sml,smr),lg) 
		end;;
let insert = function (x, h) -> merge(h, NODE(0,x,EMPTY,EMPTY));;
let findMin = function EMPTY -> raise EmptyHeap | NODE(_,x,_,_) -> x;;
let deleteMin = function EMPTY -> raise EmptyHeap
	|NODE(_,x,lh,rh) -> merge(lh, rh);;