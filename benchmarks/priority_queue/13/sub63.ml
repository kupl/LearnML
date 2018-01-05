type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

let rank = function EMPTY -> 0
| NODE(r,_,_,_) -> r


let rec merge(lh, rh) =
        match lh, rh with
        | _,EMPTY -> lh
        | EMPTY,_ -> rh
        | NODE(r1, v1, lh1, rh1), NODE(r2, v2, lh2, rh2)->
                if v1 <= v2 then NODE(((rank (merge(rh1, rh)))+1),v1,lh1,merge(rh1, rh))
                else NODE((rank rh)+1, v2,merge(lh,lh2) , rh2)

exception EmptyHeap
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))
let findMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,_,_) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
| NODE(_,x,lh,rh) -> merge(lh,rh)

let shake = function (x,lh,rh) ->
	if (rank lh) >= (rank rh)
	then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)
