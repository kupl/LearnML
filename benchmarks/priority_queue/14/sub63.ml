type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function EMPTY -> -1 | NODE(r,_,_,_)->r
let rec merge (l,r) = match (l,r) with
  | (EMPTY,EMPTY) -> EMPTY
  | (EMPTY,rh) -> rh
  | (lh,EMPTY) -> lh
  | (NODE(n1,v1,l1,r1),NODE(n2,v2,l2,r2)) -> 
      let p = 
        if v1 < v2 then NODE(n1,v1,l1,merge(r1,NODE(n2,v2,l2,r2)))
        else NODE(n2,v2,l2,merge(r2,NODE(n1,v1,l1,r1))) in
      match p with
      | EMPTY -> EMPTY
      | NODE(pn,pv,EMPTY,pr) -> NODE(pn,pv,pr,EMPTY)
      | NODE(pn,pv,pl,pr) -> shake(pv,pl,pr)
and shake = function (x,lh,rh) -> 
  if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh)
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
let findMin = function EMPTY -> raise EmptyHeap | NODE(_,x,_,_) -> x
let deleteMin = function EMPTY -> raise EmptyHeap | NODE(_,x,lh,rh) -> merge(lh,rh)