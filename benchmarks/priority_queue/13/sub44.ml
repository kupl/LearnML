type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function EMPTY -> -1
                  | NODE (r, _, _, _) -> r
let shake = function (x, lh, rh) ->
  if (rank lh) >= (rank rh)
  then NODE (rank rh + 1, x, lh, rh)
  else NODE (rank lh + 1, x, rh, lh)

let findMin = function EMPTY -> raise EmptyHeap
                  | NODE (_, x, _, _) -> x

let rec merge (h1, h2) =
  match (h1, h2) with
  | (EMPTY, EMPTY) -> EMPTY
  | (_, EMPTY) -> h1
  | (EMPTY, _) -> h2
  | (NODE (r1, x1, lh1, rh1), NODE (r2, x2, lh2, rh2)) ->
      if (x1 > x2) then merge (h2, h1)
      else
        (match (lh1, rh1) with
        | (EMPTY, EMPTY) -> shake (x1, h2, EMPTY)
        | (_, EMPTY) -> shake (x1, lh1, h2)
        | (EMPTY, _) -> shake (x1, rh1, h2)
        | _ -> NODE (r1, x1, merge (lh1, h2), rh1))

let insert = function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))
let deleteMin = function EMPTY -> raise EmptyHeap
                  | NODE (_, x, lh, rh) -> merge(lh, rh)
