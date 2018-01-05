(* base definitions *)
type heap = EMPTY
  | NODE of rank * value * heap * heap
and rank = int
and value = int;;

(* base functions *)
exception EmptyHeap ;;
let rank = function
  | EMPTY -> 0
  | NODE(r, _, _, _) -> r ;;
let findMin = function EMPTY -> raise EmptyHeap
  | NODE(_, x, _, _) -> x
and shake = function (x, lh, rh) ->
  if (rank lh) >= (rank rh)
  then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)
and leftChildHeap = function
  | EMPTY -> raise EmptyHeap
  | NODE(_, _, left, _) -> left 
and rightChildHeap = function
  | EMPTY -> raise EmptyHeap
  | NODE(_, _, _, right) -> right;;

(* heap operation *)
let rec merge (lh, rh) =
  let asc_order (a, b, test_fun) =
    if test_fun(a) < test_fun(b) then (a, b) else (b, a)
  and merge_procedure (heap_x, heap_y) =
    (* findMin(heap_x) < findMin(heap_y) guaranted *)
    shake (findMin(heap_x), 
          leftChildHeap(heap_x), 
          merge(rightChildHeap(heap_x), heap_y)
          )
  in
  match (lh, rh) with
  | (EMPTY, EMPTY) -> EMPTY
  | (EMPTY, _) -> rh
  | (_, EMPTY) -> lh
  | _ -> merge_procedure(asc_order(lh, rh, findMin)) ;;
let insert = function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY)) ;;
let deleteMin = function EMPTY -> raise EmptyHeap
  | NODE(_, _, lh, rh) -> merge(lh, rh) ;;

(* test_case *)
assert(findMin(deleteMin(deleteMin(insert(6, insert(4, insert(9, insert(5, insert(3, insert(-1, EMPTY))))))))) = 4);;
