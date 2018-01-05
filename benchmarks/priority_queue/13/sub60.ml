type rank = int

type value = int

type heap = EMPTY | NODE of rank * value * heap * heap

exception EmptyHeap

let rank = function EMPTY -> -1
				  | NODE(r, _, _, _) -> r

let shake = function (x, lh, rh) ->
  if (rank lh) >= (rank rh) then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

let findMin = function EMPTY -> raise EmptyHeap
					 | NODE(_, x, _, _) -> x

let rec merge (x, y) =
  match (x, y) with
  | (EMPTY, EMPTY) -> EMPTY
  | (h, EMPTY) -> h
  | (EMPTY, h) -> h
  | (NODE(r1, x1, lh1, rh1), NODE(r2, x2, lh2, rh2)) -> if x1 < x2 then
  														  let h_merged = merge(rh1, NODE(r2, x2, lh2, rh2)) in
														  shake(x1, lh1, h_merged)
														else merge(y, x)

let insert = function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
					   | NODE(_, x, lh, rh) -> merge(lh, rh)
