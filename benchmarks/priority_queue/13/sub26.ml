type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
                | NODE(r,_,_,_) -> r

let findMin = function EMPTY -> raise EmptyHeap
                     | NODE(_,x,_,_) -> x
let shake = function (x, lh, rh) ->
            if (rank lh) >= (rank rh)
            then NODE(rank rh + 1, x, lh, rh)
            else NODE(rank lh + 1, x, rh, lh)
let leftHeap = function EMPTY -> raise EmptyHeap
                | NODE(_,_,h,_) -> h
let rightHeap = function EMPTY -> raise EmptyHeap
                | NODE(_,_,_,h) -> h

let rec merge (h1, h2) = 
        match (h1, h2) with
        | (EMPTY, h) -> h
        | (h, EMPTY) -> h
        | (h1, h2) ->
          if ((findMin h1) > (findMin h2))
          then shake((findMin h2), (leftHeap h2) , merge(h1, (rightHeap h2)))
          else shake((findMin h1), (leftHeap h1), merge(h2, (rightHeap h1)))
               
let insert = function (x, h) -> merge (h, NODE(0, x, EMPTY, EMPTY))
let deleteMin = function EMPTY -> raise EmptyHeap
                       | NODE(_,x,lh,rh) -> merge(lh, rh)