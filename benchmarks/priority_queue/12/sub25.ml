type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap 

let rank h = match h with 
  | EMPTY -> -1 
  | NODE(r,_,_,_) -> r 

let shake (x,lh,rh) = 
  if (rank lh) >= (rank rh) 
  then NODE(rank rh+1, x, lh, rh) 
  else NODE(rank lh+1, x, rh, lh) 

let findMin h = match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_,x,_,_) -> x 

let rec merge (lh,rh) = 
	match (lh,rh) with
	|(EMPTY,_) -> rh
	|(_,EMPTY) -> lh
	|(_,_) ->
			( let rhmin = findMin rh in
			  let lhmin = findMin lh in
			  let rhdel = deleteMin rh in
			  let lhdel = deleteMin lh in
			  if rhmin <= lhmin then (shake (rhmin,lh,rhdel))
			  else (shake (lhmin, rh, lhdel))
			)
	
and deleteMin h = match h with
	  | EMPTY -> raise EmptyHeap 
	  | NODE(_,x,lh,rh) -> merge (lh,rh)

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

