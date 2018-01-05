exception EmptyHeap

type rank = int
type value = int
type heap = EMPTY
|NODE of rank * value * heap * heap

let rank = function EMPTY -> -1 
|NODE(r,_,_,_) -> r

let findMin = function EMPTY -> raise EmptyHeap
|NODE(r,a,b,c) -> a

let shake = function (x, lh,rh) ->
  if(rank lh) >= (rank rh)
  then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

let rec merge(lh, rh) = match (lh, rh) with
|(EMPTY, _ ) -> rh
|( _ , EMPTY) -> lh
|(NODE(lr, lv, llh, lrh), NODE(rr, rv, rlh, rrh)) ->
  if(lv <= rv)
  then shake(lv, merge(llh,lrh), rh)
  else shake(rv, lh, merge(rlh, rrh))

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
 |NODE(a,x,lh,rh) -> merge(lh,rh)




