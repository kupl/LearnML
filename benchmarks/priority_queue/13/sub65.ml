type heap = EMPTY | NODE of rank * value * heap * heap 
and rank=int
and value=int
  exception EmptyHeap
let rank = function EMPTY -> 0
| NODE(r,_,_,_) -> r
let findMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,_,_) -> x let shake = function (x,lh,rh) ->
if (rank lh) >= (rank rh)
then NODE(rank rh + 1, x, lh, rh)
else NODE(rank lh + 1, x, rh, lh)
let rec merge = function (lh, rh) ->
  if lh = EMPTY then rh
  else if rh = EMPTY then lh
  else if findMin(lh) <= findMin(rh)
  then 
let deleteMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,lh1,rh1) -> merge(lh1,rh1) in
shake(findMin(lh),deleteMin(lh), rh)
  else 
let deleteMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,lh1,rh1) -> merge(lh1,rh1) in
shake(findMin(rh), deleteMin(rh), lh)

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,lh1,rh1) -> merge(lh1,rh1)
