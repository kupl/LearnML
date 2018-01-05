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

let comp = function (lh, rh) ->
    match lh with
    | EMPTY -> false
    | NODE (_,v,_,_) ->
            match rh with
            |EMPTY -> true
            |NODE (_,v2,_,_) -> (v>v2)

let rec merge = function (lh, rh) ->
    match lh with
    | EMPTY -> rh
    | NODE (lr, lv, llh, lrh) ->
        if (comp(lh,rh)) then merge(rh, lh)
        else shake(lv, llh, merge(lrh,rh))

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
                    | NODE(_,x,_,_) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
                       | NODE(_,x,lh,rh) -> merge(lh,rh)

