type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = 
  function EMPTY -> -1
    | NODE(r,_,_,_) -> r 

let findMin = 
  function EMPTY -> raise EmptyHeap
    | NODE(_,x,_,_) -> x

let shake = 
  function (x,lh,rh) ->
    if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh) 

let rec merge x = 
  match x with
  | (EMPTY, EMPTY) -> EMPTY
  | (EMPTY, rh) -> rh
  | (lh, EMPTY) -> lh
  | (NODE(r1, v1, lh1, lh2), NODE(r2, v2, rh1, rh2)) ->
      if v1 <= v2 then
	  if (rank lh1) = (rank lh2) then
	    merge ( shake(v1, merge(lh1, NODE(0, v2, EMPTY, EMPTY)), lh2), merge(rh1, rh2))
	  else
	    merge ( shake(v1, merge(lh2, NODE(0, v2, EMPTY, EMPTY)), lh1) , merge(rh1, rh2))
      else merge (NODE(r2, v2, rh1, rh2), NODE(r1, v1, lh1, lh2))

let insert = 
  function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin =   
  function EMPTY -> raise EmptyHeap
    | NODE (_,x,lh,rh) -> merge(lh, rh)
