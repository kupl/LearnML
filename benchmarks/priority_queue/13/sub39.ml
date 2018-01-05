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

let findMin = function EMPTY -> raise EmptyHeap
                     | NODE(_, x, _, _) -> x

let rec merge (lh, rh) =
  let leftHeap hp =
    match hp with
    | EMPTY -> raise EmptyHeap
    | NODE(_, _, _lh, _) -> _lh in
  let rightHeap hp =
    match hp with
    | EMPTY -> raise EmptyHeap
    | NODE(_, _, _, _rh) -> _rh in

  if lh = EMPTY then rh
  else if rh = EMPTY then lh
  else if findMin lh <= findMin rh then
    shake (findMin lh, leftHeap lh, merge (rightHeap lh, rh))
  else
    shake (findMin rh, leftHeap rh, merge (rightHeap rh, lh))

let insert = function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))
let deleteMin = function EMPTY -> raise EmptyHeap
                       | NODE(_, _, lh, rh) -> merge(lh, rh)
