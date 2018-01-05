exception EmptyHeap

type heap = EMPTY
	    | NODE of rank * value * heap * heap
and rank = int
and value = int

let rank = function EMPTY -> -1 
                    | NODE (r, _, _, _) -> r

let findMin = function EMPTY -> raise EmptyHeap
		       | NODE(_,x,_,_) -> x


let shake = function (x, lh, rh) ->
  if (rank lh) >= (rank rh)
  then NODE(rank rh +1, x, lh, rh)
  else NODE(rank lh +1, x, rh, lh)
 
let getLh (h: heap): heap = 
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_, _, lh, _) -> lh

let getRh (h: heap): heap = 
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_, _, _, rh) -> rh

let rec merge ((h1: heap), (h2: heap)): heap = 
  match h1, h2 with
  | EMPTY, _ -> h2
  | _, EMPTY -> h1
  | _ -> if (findMin h1) <= (findMin h2) 
	 then shake( (findMin h1), (getLh h1), (merge ((getRh h1), h2)) )
	 else shake( (findMin h2), (getLh h2), (merge ((getRh h2), h1)) )

let insert = function (x,h) -> merge(h, NODE(0, x, EMPTY, EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
		       | NODE(_,x,lh,rh) -> merge(lh,rh)
