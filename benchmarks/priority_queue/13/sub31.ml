type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int


exception EmptyHeap

let rank = function EMPTY -> -1 
                  | NODE (r, _, _, _) -> r


let shake = function (x,lh,rh) ->
if (rank lh) >= (rank rh)
then NODE(rank rh + 1, x, lh, rh)
else NODE(rank lh + 1, x, rh, lh)



let rec merge (heap1, heap2) = 
	match (heap1, heap2) with
		(EMPTY, EMPTY) -> EMPTY
		| (EMPTY, h) -> h
		| (h, EMPTY) -> h
		| (NODE(r1, v1, h11, h12), NODE(r2, v2, h21, h22)) -> 
			if (v1 < v2) then shake (v1, (merge (h11, h12)), NODE(r2, v2, h21, h22))
			else shake (v2, NODE(r1, v1, h11, h12), (merge (h21, h22)))


let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
			| NODE(_ ,x,_ ,_) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
			| NODE(_ ,x,lh,rh) -> merge(lh,rh)


