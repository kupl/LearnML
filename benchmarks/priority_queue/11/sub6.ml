type heap = EMPTY | NODE of rank * value * heap * heap 
and rank = int 
and value = int 

exception EmptyHeap 

let rank h = match h with 
  | EMPTY -> -1 
  | NODE(r,_,_,_) -> r 
(*10*)
let shake (x,lh,rh) = 
  if (rank lh) >= (rank rh) 
  then NODE(rank rh+1, x, lh, rh) 
  else NODE(rank lh+1, x, rh, lh) 

let rec merge (heap1, heap2) =
 	match heap1 with
	| EMPTY ->
		begin
(*20*)		match heap2 with
		| EMPTY -> EMPTY
		| NODE(r,v,lh,rh) -> heap2
		end
	| NODE(r,v,lh,rh) ->
		begin
		match heap2 with
		| EMPTY -> heap1
		| NODE(r',v',lh',rh') ->
			begin
(*30*)			if v < v' then
				begin
				match lh with
				| EMPTY -> 
					begin
					match rh with
					| EMPTY -> shake (v, rh, heap2)
					| NODE(_,_,_,_) -> shake (v, rh, heap2)
					end
				| NODE(_,lhv,_,_) -> 
(*40*)					begin
					match rh with
					| EMPTY -> shake (v, lh, heap2)
					| NODE(_,rhv,_,_) -> 
						begin
						if v'<lhv then merge((shake (v,heap2,rh)), lh)
						else if (rank lh) < (rank rh) then merge((shake (v, lh, rh)), heap2)
						else merge((shake (v, (merge (lh, heap2)), rh)), heap2)
						end
					end
				end
			else merge(heap2, heap1)
			end
		end	

let findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,_,_) -> x 

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,lh,rh) -> merge (lh,rh) 
