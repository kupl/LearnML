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

let rec merge (lh, rh) = 
        match (lh, rh) with
          (EMPTY, _) -> rh
        | (_, EMPTY) -> lh
        | (NODE(_, x, lh1, rh1), NODE(_, y, lh2, rh2)) ->
                if x <= y
                 then shake (x, lh1, merge (rh1, rh))
                 else shake (y, lh2, merge (lh, rh2))  

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
                       | NODE(_,x,lh,rh) -> merge(lh,rh)
