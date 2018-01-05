(* C:\OCaml\lib\leftist heap.ml *)

exception EmptyHeap

type rank = int;;
type value = int;;
type heap = EMPTY | NODE of rank * value * heap * heap;;

let rank = function EMPTY -> -1
  |NODE(r, _, _, _) -> r

let shake = function (x,lh,rh) ->
  if (rank lh) >= (rank rh)
  then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

let rec merge (heap1, heap2) =
  match (heap1, heap2) with
  |(EMPTY, _) -> heap2
  |(_, EMPTY) -> heap1
  |(NODE (r1, x, lh1, rh1), NODE (r2, y, lh2, rh2)) -> 
      (if (x - y < 0) then 
	(if (lh1 = EMPTY) then shake (x, merge(rh1, NODE(r2, y, lh2, rh2)), lh1)
	    else shake (x, lh1, merge(rh1, NODE(r2, y, lh2, rh2))))
	  else
	(if (lh2 = EMPTY) then shake (y, merge (rh2, NODE(r1, x, lh1, rh1)), lh2)
	    else shake (y, lh2, merge (rh2, NODE(r1, x, lh1, rh1)))))

let insert = function (x, h) -> merge(h, NODE (0, x, EMPTY, EMPTY));;
let findMin = function EMPTY -> raise EmptyHeap
  |NODE(_, x, _, _) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
  |NODE(_, x, lh, rh) -> merge(lh, rh)
