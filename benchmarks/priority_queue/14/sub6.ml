type heap =
  EMPTY
| NODE of rank * value * heap * heap
and rank =
  int
and value =
  int

exception EmptyHeap

let rank = function EMPTY -> -1 
                  | NODE (r, _, _, _) -> r 
let findMin = function EMPTY -> raise EmptyHeap
                     | NODE(_, x, _, _) -> x
let shake = function (x, lh, rh) ->
  if (rank lh) >= (rank rh)
  then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

let rec merge((h1:heap), (h2: heap)): heap = match (h1, h2) with
    (EMPTY, _) -> h2
  | (_, EMPTY) -> h1
  | (NODE(r1, v1, lh1, rh1), NODE(r2, v2, lh2, rh2)) when v1 > v2 ->
      let newRh = merge(h1, rh2) in
      shake(v2, lh2, newRh)
  | (NODE(r1, v1, lh1, rh1), NODE(r2, v2, lh2, rh2)) ->
      let newLh = merge(rh1, h2) in
      shake(v1, lh1, newLh)

let insert = function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))
let deleteMin = function EMPTY -> raise EmptyHeap
                       | NODE(_, x, lh, rh) -> merge(lh, rh)
