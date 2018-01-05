type heap =
    | EMPTY
    | NODE of rank * value * heap * heap
and rank = int
and value = int;;

exception EmptyHeap;;

let rank = function
    | EMPTY -> -1
    | NODE(r,_,_,_) -> r;;

let shake = function
    | (x,lh,rh) ->
            if (rank lh) >= (rank rh)
            then NODE(rank rh + 1, x, lh, rh)
            else NODE(rank lh + 1, x, rh, lh);;

let rec merge = function
    | (EMPTY,h) | (h,EMPTY) -> h
    | (NODE(r0,v0,lh0,rh0),NODE(r1,v1,lh1,rh1)) ->
            if v0 <= v1
            then shake(v0,lh0,merge(rh0,NODE(r1,v1,lh1,rh1)))
            else shake(v1,lh1,merge(rh1,NODE(r0,v0,lh0,rh0)));;

let insert = function
    | (x,h) -> merge(h,NODE(0,x,EMPTY,EMPTY));;

let findMin = function
    | EMPTY -> raise EmptyHeap
    | NODE(_,x,_,_) -> x;;

let deleteMin = function
    | EMPTY -> raise EmptyHeap
    | NODE(_,x,lh,rh) -> merge(lh,rh);;
