type heap = EMPTY | NODE of rank * value * heap * heap
    and rank = int
    and value = int
exception EmptyHeap
let rank = function EMPTY -> -1
  |NODE(r,_,_,_) -> r
let shake = function (x,lh,rh) ->
  if (rank lh) >= (rank rh)
      then NODE(rank rh+1, x, lh, rh)
      else NODE(rank lh+1, x, rh, lh)
let findRight = function EMPTY -> EMPTY
  |NODE(_,_,_,rh) -> rh
let findLeft = function EMPTY -> EMPTY
  |NODE(_,_,lh,_) -> lh
let findMin = function EMPTY -> raise EmptyHeap
  |NODE(_,x,_,_) -> x
let rec merge = function (lh,EMPTY) -> lh
  |(EMPTY,rh) -> rh
  |(lh,rh) -> if findMin(lh) < findMin(rh)
        then shake(findMin(lh),findLeft(lh),merge(findRight(lh),rh))
	else merge(rh,lh)
let insert = function (x,h) -> merge(h,NODE(0,x,EMPTY,EMPTY))
let deleteMin = function EMPTY -> raise EmptyHeap
  |NODE(_,x,lh,rh) -> merge(lh,rh)