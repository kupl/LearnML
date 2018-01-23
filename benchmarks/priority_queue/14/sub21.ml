type heap = EMPTY | NODE of rank * value * heap * heap
 and rank = int
 and value = int
;;

exception EmptyHeap;;

let rank = function
    EMPTY -> -1
  | NODE (r, _, _, _) -> r
;;

let shake = function
    (x, lh, rh) ->
    if (rank lh) >= (rank rh) then
      NODE (rank rh + 1, x, lh, rh)
    else NODE (rank lh + 1, x, rh, lh)
;;

let rec merge (heap_1, heap_2) =
  match (heap_1, heap_2) with
  | (heap, EMPTY) -> heap
  | (EMPTY, heap) -> heap
  | (NODE (_, x_1, lh_1, rh_1), NODE (_, x_2, _, _)) ->
    if x_1 > x_2 then
     merge(heap_2, heap_1)
    else
     shake (x_1, lh_1, merge(rh_1, heap_2))
;;

let insert = function (x, h) ->
    merge (h, NODE(0, x, EMPTY, EMPTY))
;;

let findMin = function
    EMPTY -> raise EmptyHeap
  | NODE (_, x, _, _) -> x
;;

let deleteMin = function
    EMPTY -> raise EmptyHeap
  | NODE (_, x, lh, rh) -> merge(lh, rh)
;;
