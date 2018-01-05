type heap=EMPTY
| NODE of rank * value * heap * heap
and rank=int
and value=int

exception EmptyHeap

let rank=function EMPTY -> -1
|NODE(r,a,b,c) -> r



let shake= function (x, lh, rh) ->
	if(rank lh) >= (rank rh)
	then NODE(rank rh+1, x, lh, rh)
	else NODE(rank lh+1, x, rh, lh)

let rec merge (x, y)=
match (x, y) with
|(a, EMPTY) -> a
|(EMPTY, b)->b
|(NODE(arank, aval, aleft, aright), NODE(brank, bval, bleft, bright))->
if aval >= bval then 
	shake(bval, merge(bleft, bright), NODE(arank, aval, aleft, aright))
else
	shake(aval, merge(aleft, aright), NODE(brank, bval, bleft, bright))


let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
| NODE(r,x,b,c) -> x

let deleteMin = function EMPTY -> raise EmptyHeap
| NODE( r,x,lh,rh) -> merge(lh,rh)

