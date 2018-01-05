type heap = 
      EMPTY 
    | NODE of rank * value * heap * heap
and rank = int 
and value = int 

exception EmptyHeap

let rank : heap -> rank = function 
      EMPTY -> -1  
    | NODE (r, _, _, _) -> r 

let findMin : heap -> value = function 
      EMPTY -> raise EmptyHeap
    | NODE(_, x, _, _) -> x

let shake : value * heap * heap -> heap = function (x,lh,rh) ->
    if (rank lh) >= (rank rh) 
    then NODE(rank rh + 1, x, lh, rh) 
    else NODE(rank lh + 1, x, rh, lh) 

let heap_val : heap -> value = function 
      EMPTY -> 0
    | NODE (_, x, _, _) -> x

let rec merge : heap * heap -> heap = fun (h1, h2) ->
    match (h1, h2) with
    | (EMPTY, _) -> h2
    | (_, EMPTY) -> h1
    | (NODE (r1, v1, lh1, rh1), NODE (r2, v2, lh2, rh2)) -> 
        if v1 <= v2 then shake (v1, lh1, (merge (rh1, h2)))
        else shake (v2, lh2, (merge (rh2, h1)))

let insert : value * heap -> heap = function (x,h) ->  
    merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin : heap -> heap = function 
      EMPTY -> raise EmptyHeap
    | NODE(_ ,x,lh,rh) -> merge(lh,rh)
