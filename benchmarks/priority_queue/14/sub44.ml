(* hw2-2, 2012-11259 *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank: heap -> rank =
  fun h -> match h with
    | EMPTY -> -1
    | NODE(r, _, _, _) -> r

let findMin: heap -> value =
  fun h -> match h with
    | EMPTY -> raise EmptyHeap
    | NODE(_, x, _, _) -> x

let snake: value * heap * heap -> heap =
  fun (x, lh, rh) ->
    if (rank lh) >= (rank rh) 
      then NODE(rank rh + 1, x, lh, rh) 
      else NODE(rank lh + 1, x, rh, lh) 

let rec merge: heap * heap -> heap =
  fun (h, h') -> match (h, h') with
    | (EMPTY, _) -> h'
	| (_, EMPTY) -> h
	| (NODE(_, x, lh, rh), NODE(_, x', _, _)) ->
	  if x > x'
	    then merge(h', h)
		else snake(x, lh, merge(rh, h'))

let insert: value * heap -> heap =
  fun (x, h) ->
    merge(h, NODE(0, x, EMPTY, EMPTY))

let deleteMin: heap -> heap =
  fun h -> match h with
    | EMPTY -> raise EmptyHeap
	| NODE(_, _, lh, rh) -> merge(lh, rh)
