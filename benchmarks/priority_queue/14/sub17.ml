(* hw2-2 *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function
    EMPTY -> -1
  | NODE (r, _, _, _) -> r
let findMin = function
    EMPTY -> raise EmptyHeap
  | NODE (_, x, _, _) -> x

let shake = function (x, lh, rh) ->
  if (rank lh) >= (rank rh) then
    NODE (rank rh + 1, x, lh, rh)
  else
    NODE (rank lh + 1, x, rh, lh)
let rec merge (a, b) =
  if a = EMPTY then
    b
  else if b = EMPTY then
    a
  else if (findMin a) <= (findMin b) then
    match a with
      EMPTY -> b (* arienai *)
    | NODE (_, v, lh, rh) -> shake (v, lh, merge (rh, b))
  else
    match b with
      EMPTY -> a (* arienai *)
    | NODE (_, v, lh, rh) -> shake (v, lh, merge (rh, a))

let insert = function (x, h) -> merge (h, NODE (0, x, EMPTY, EMPTY))
let deleteMin = function
    EMPTY -> raise EmptyHeap
  | NODE (_, x, lh, rh) -> merge (lh, rh)
