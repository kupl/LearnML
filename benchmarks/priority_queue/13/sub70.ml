type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function EMPTY -> -1
| NODE(r,_,_,_) -> r

let shake = function (x,lh,rh) ->
if (rank lh) >= (rank rh)
then NODE(rank rh + 1, x, lh, rh)
else NODE(rank lh + 1, x, rh, lh)

let rec merge (hp1, hp2) =
  match hp1, hp2 with
  | EMPTY, _ -> hp2
  | _, EMPTY -> hp1
  | NODE (_, x, a1, b1), NODE (_, y, a2, b2) -> 
        if x <= y then shake(x,a1,merge(b1,hp2) )
        else shake(y,a2,merge(hp1,b2) )

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
let findMin = function EMPTY -> raise EmptyHeap
| NODE( _,x,_ ,_ ) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
| NODE(_ ,x,lh,rh) -> merge(lh,rh)

