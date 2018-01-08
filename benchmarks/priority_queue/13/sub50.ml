type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
		  | NODE(r, _, _, _) -> r

let shake = function (x, lh, rh) ->
	if (rank lh) >= (rank rh) then NODE (rank rh + 1, x, lh, rh)
	else NODE (rank lh + 1, x, rh, lh)

let rec merge heaps = 
	match heaps with
		| (EMPTY, rheap) -> rheap
		| (lheap, EMPTY) -> lheap
		| (NODE (lrank, lvalue, ll, lr), NODE (rrank, rvalue, rl, rr)) ->
			if lvalue > rvalue then shake (rvalue, NODE (lrank, lvalue, ll, lr), merge (rl, rr))
			else shake (lvalue, merge (ll, lr), NODE (rrank, rvalue, rl, rr))
	
let insert = function (x, h) -> merge (h, NODE(0, x, EMPTY, EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
		     | NODE(_, x, _, _) -> x

let deleteMin = function EMPTY -> raise EmptyHeap
		       | NODE (_, x, lh, rh) -> merge (lh, rh)
