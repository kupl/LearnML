type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int;;

exception EmptyHeap


let rank = function EMPTY -> -1
        | NODE(r, _, _, _) -> r;;

let shake = function (x, lh, rh) ->
    if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh);;

let rec merge (heap1, heap2) = match heap1, heap2 with
    lh, EMPTY -> 
            heap1 (*don't have to merge*)
    | EMPTY, rh -> merge(heap2, EMPTY)
    | NODE(_, x, leftlh, rightlh), NODE(_, y, leftrh, rightrh) ->
            if (x <= y) then shake(x, leftlh, merge(rightlh, heap2))
            else shake(y, leftrh, merge(heap1, rightrh))
            (* idea by wikipedia - leftist heap *)
;;

let insert = function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY));;
let findMin = function EMPTY -> raise EmptyHeap
            | NODE(_, x, _, _) -> x;;
let deleteMin = function EMPTY -> raise EmptyHeap
            | NODE(_, x, lh, rh) -> merge(lh, rh);;
