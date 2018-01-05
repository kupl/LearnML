type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int
exception EmptyHeap
let rank = function EMPTY -> -1 
                  | NODE (r, _, _, _) -> r 

let shake = function (x,lh,rh) ->
if (rank lh) >= (rank rh)
then NODE(rank rh + 1, x, lh, rh)
else NODE(rank lh + 1, x, rh, lh)
let rec merge (heap1,heap2) : heap =
match (heap1,heap2) with
  | (EMPTY,NODE(_,_,_,_)) -> heap2
  | (NODE(_,_,_,_),EMPTY) -> heap1
  | (NODE(x1,v1,l1,r1),NODE(x2,v2,l2,r2)) -> if v1<=v2 then shake(v1,l1,merge(NODE(x2,v2,l2,r2), r1))
else shake(v2,l2,merge (NODE(x1,v1,l1,r1), r2))

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
let findMin = function EMPTY -> raise EmptyHeap
| NODE( _,x,_ ,_ ) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
  | NODE(_ ,x,lh,rh) -> merge(lh,rh)