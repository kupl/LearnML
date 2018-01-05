type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int
exception EmptyHeap
let rank = function EMPTY -> -1
	| NODE(r, a, b, c) -> r
let findMin = function EMPTY -> raise EmptyHeap
| NODE(a,x, c, d) -> x
let leftheap = function EMPTY -> raise EmptyHeap
						| NODE( a, b, lh, rh) -> lh
let rightheap = function EMPTY -> raise EmptyHeap
						| NODE( a,b , lh, rh) -> rh


let shake = function (x,lh,rh) ->
if (rank lh) >= (rank rh)
then NODE(rank rh + 1, x, lh, rh)
else NODE(rank lh + 1, x, rh, lh)

let rec merge : heap * heap -> heap = 
	fun f -> 
		match f with 
			|(EMPTY, a) -> a
			|(a, EMPTY) -> a
			|(a, b) -> if ((findMin a) > (findMin b)) then (merge (b, a))
					else if (rank (leftheap a)) == -1 then (shake ((findMin a), (leftheap a), b))
					else (shake ((findMin a) ,(leftheap a) ,(merge ((rightheap a), b))))
let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
						| NODE( a,x,lh,rh) -> merge(lh,rh)

