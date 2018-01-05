type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function EMPTY -> 0 | NODE(r,_,_,_) -> r
let findMin = function EMPTY -> raise EmptyHeap | NODE(_,x,_,_) -> x

let shake = function (x,lh,rh) ->
        if (rank lh) >= (rank rh)
                then NODE(rank rh + 1, x, lh, rh)
                else NODE(rank lh + 1, x, rh, lh)

let _shake = function (x,lh,rh) ->
        if lh == EMPTY then NODE(0,x,rh,lh)
        else if rh == EMPTY then NODE(0,x,lh,rh)
        else shake(x,lh,rh)
        

let rec merge(h1, h2) =
        match (h1,h2) with
        |(EMPTY,h2) -> h2
        |(h1,EMPTY) -> h1
        |(NODE(r1,x1,lh1,rh1),NODE(r2,x2,lh2,rh2)) -> 
                        if(x1 <= x2)
                                then _shake(x1,lh1,merge(rh1,h2))
                                else _shake(x2,lh2,merge(rh2,h1))
                                (*then if (rank lh1) > (rank rh1)
                                        then _shake(x1,merge(rh1,h2),lh1)
                                        else _shake(x1,merge(lh1,h2),rh1)
                                else if (rank lh2) > (rank rh2)
                                        then _shake(x2,merge(rh2,h1),lh2)
                                        else _shake(x2,merge(lh2,h1),rh2)*)

 
let deleteMin = function EMPTY -> raise EmptyHeap | NODE(_,x,lh,rh) -> merge(lh,rh)
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))


