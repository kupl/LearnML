type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function EMPTY -> -1
				  | NODE(r, _, _, _) -> r

let shake = function (x, lh, rh) ->
		if (rank lh) >= (rank rh)
		 then NODE(rank rh + 1, x, lh, rh)
		 else NODE(rank lh + 1, x, rh, lh)

let rec merge = function EMPTY, EMPTY -> EMPTY
				   | EMPTY, rh -> rh
				   | lh, EMPTY -> lh
				   | NODE(xr, x, xlh, xrh), NODE(yr, y, ylh, yrh) ->
				   if (x <= y)
				    then shake(x, xlh, merge(xrh, NODE(yr, y, ylh, yrh)))
					else shake(y, ylh, merge(yrh, NODE(xr, x, xlh, xrh)))

let insert = function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))
let findMin = function EMPTY -> raise EmptyHeap
					 | NODE(_, x, _, _) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
					   | NODE(_, x, lh, rh) -> merge(lh, rh)
