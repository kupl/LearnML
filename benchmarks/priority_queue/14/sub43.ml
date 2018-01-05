
(* Author: Arif Jafer, 2012-11255 *)
(* PL, Spring 2014 *)

(* HW2-Q2: Left Priority Heap *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rec insert = function (x,h) ->
  merge(h, NODE(0,x,EMPTY,EMPTY))

and merge = function
  | (EMPTY, x) | (x, EMPTY) -> x
  | (lhs, rhs) ->
      if (findMin lhs) <= (findMin rhs)
      then shake ((findMin lhs), (deleteMin lhs), rhs)
      else shake ((findMin rhs), lhs, (deleteMin rhs))

and rank = function
  | EMPTY -> -1
  | NODE(r,_,_,_) -> r

and findMin = function
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,_,_) -> x

and shake = function (x,lh,rh) ->
  if (rank lh) >= (rank rh)
  then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

and deleteMin = function
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,lh,rh) -> merge(lh,rh)



