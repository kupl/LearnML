type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1 
                  | NODE (r, _, _, _) -> r 
let findMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,_,_) -> x
let getl = function EMPTY -> raise EmptyHeap | NODE(_,_,l,_) -> l
let getr = function EMPTY -> raise EmptyHeap | NODE(_,_,_,r) -> r
let shake = function (x,lh,rh) ->
if (rank lh) >= (rank rh)
then NODE(rank rh + 1, x, lh, rh)
else NODE(rank lh + 1, x, rh, lh)


(*
1. check who is larger 
2. larger side goes down
*)
let rec merge = function (a, b) ->
  if (rank a = -1) then b
  else if (rank b = -1) then a
  else
    if (findMin a < findMin b) then
      if (rank a = 0) then shake(findMin a, b, EMPTY) else shake(findMin a, getl a, merge(getr a, b))
    else
      if (rank b = 0) then shake(findMin b, a, EMPTY) else shake(findMin b, getl b, merge(getr b, a))
  

let deleteMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,lh,rh) -> merge(lh,rh)
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))



