type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function EMPTY -> -1
                  | NODE(r,_,_,_) -> r

let findMin = function EMPTY -> raise EmptyHeap
                     | NODE(_,x,_,_) -> x

let shake = function (x,lh,rh) ->
    if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh)

let rec merge (a, b) =
    match (a, b) with
    | (EMPTY, _) -> b
    | (_, EMPTY) -> a
    | (NODE(an, av, al, ar), NODE(bn, bv, bl, br)) -> if findMin a > findMin b
    then merge (b, a) else shake (av, al, merge (ar, b)) 

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
let deleteMin = function EMPTY -> raise EmptyHeap
                       | NODE(_,x,lh,rh) -> merge(lh,rh)
