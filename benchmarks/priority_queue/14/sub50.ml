type rank = int
type value = int
type heap = EMPTY | NODE of rank * value * heap * heap

exception EmptyHeap

let rank =
function EMPTY -> -1
       | NODE (r,_,_,_) -> r

let findMin =
function EMPTY -> raise EmptyHeap
       | NODE (_,x,_,_) -> x

let shake =
function (x, lh, rh) -> if (rank lh) >= (rank rh)
                           then NODE (rank rh+1, x, lh, rh)
						   else NODE (rank lh+1, x, rh, lh)

let rec merge =
function (EMPTY, h) -> h
       | (h, EMPTY) -> h
       | (h1, h2) -> if findMin h1 >= findMin h2 then merge (h2, h1)
	                 else match h1 with NODE (r,x,lh,rh) -> shake (x, merge(lh, rh), h2)

let insert =
function (x, h) -> merge (h, NODE(0, x, EMPTY, EMPTY))

let deleteMin =
function EMPTY -> raise EmptyHeap
       | NODE (_,x,lh,rh) -> merge (lh,rh)
