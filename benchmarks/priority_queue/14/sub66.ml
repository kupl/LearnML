type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
	|NODE(r,_,_,_) -> r

let value x = 
	match x with
	|EMPTY -> raise EmptyHeap
	|NODE(_,a,_,_) -> a

let shake = function (x,lh,rh) ->
        if (rank lh) >= (rank rh)
        then NODE(rank rh + 1, x, lh, rh)
        else NODE(rank lh + 1, x, rh, lh)

let findMin = function EMPTY -> raise EmptyHeap

let rc x =
        match x with
	|EMPTY -> raise EmptyHeap
        |NODE(a, b, c, d) -> d

let lc x =
        match x with
	|EMPTY -> raise EmptyHeap
        |NODE(a, b, c, d) -> c

let rec merge (a, b) =
        if a = EMPTY then b
        else if b = EMPTY then a
        else if (value a) > (value b) then merge(b, a)
        else shake(value a, lc a, merge(rc a, b))


let deleteMin = function EMPTY -> raise EmptyHeap

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

