(* hw 2-2 *)
(* 2012-11269 DongJae Lim *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let findMin = function EMPTY -> raise EmptyHeap
                     | NODE(_, x, _, _) -> x

let rank = function EMPTY -> -1
                  | NODE (r, _, _, _) -> r

let shake = function (x,lh,rh) ->
  if (rank lh) >= (rank rh)
  then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

let leftHeap (h : heap) : heap =
  match h with
  | EMPTY -> EMPTY
  | NODE (r, v, lh, rh) -> lh

let rightHeap (h : heap) : heap =
  match h with
  | EMPTY -> EMPTY
  | NODE (r, v, lh, rh) -> rh

let rec merge ((lh : heap), (rh : heap)) : heap =
  match lh, rh with
  | EMPTY, EMPTY -> EMPTY
  | _, EMPTY -> lh
  | EMPTY, _ -> rh
  | _, _ ->
    let lv = (findMin lh) in
    let rv = (findMin rh) in
    if lv < rv
    then shake (lv, (leftHeap lh), merge((rightHeap lh), rh))
    else shake (rv, (leftHeap rh), merge(lh, (rightHeap rh)))

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
                       | NODE(_,x,lh,rh) -> merge(lh,rh)
