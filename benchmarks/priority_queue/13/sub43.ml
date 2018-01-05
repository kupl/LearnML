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

let rec merge (lh, rh) = 
	match (lh, rh) with 
	| (EMPTY, _) -> rh
	| (_, EMPTY) -> lh
	| (NODE(_, v1, left_lh, left_rh), NODE(_, v2, right_lh, right_rh)) -> if (v1 < v2) then (shake (v1, left_lh, (merge (left_rh, rh))))
																		  else (merge (rh, lh))


let insert = function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
							 | NODE(_, x, _, _) -> x

let deleteMin = function EMPTY -> raise EmptyHeap
							   | NODE(_, x, lh, rh) -> merge (lh, rh)



