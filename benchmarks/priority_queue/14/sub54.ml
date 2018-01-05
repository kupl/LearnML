type heap = EMPTY | NODE of rank * value * heap * heap 
and rank = int
and value = int
;;

exception EmptyHeap
let rank = function EMPTY -> -1
        | NODE(r,_,_,_) -> r
let findMin = function EMPTY -> raise EmptyHeap
            | NODE(_,x,_,_) -> x
let shake = function (x,lh,rh) ->
             if (rank lh) >= (rank rh) then 
            NODE(rank rh + 1, x, lh, rh)
            else NODE(rank lh + 1, x, rh, lh)

let rec merge (x,y) =
            if x = EMPTY then y
            else if y = EMPTY then x
            else if findMin x > findMin y then
              merge (y,x)
            else
              match x with
              |EMPTY -> raise EmptyHeap
              |NODE(_,v,left,right) -> shake (v,left,merge(right,y))
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
let deleteMin = function EMPTY -> raise EmptyHeap
              | NODE(_,x,lh,rh) -> merge(lh, rh)
;;
