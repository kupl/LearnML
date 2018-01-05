type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rec rank = function
  | EMPTY -> -1
  | NODE(r,_,_,_) -> r

(* ���ο� NODE�� �����. *)
and shake = function (x,lh,rh) ->
  if (rank lh) >= (rank rh)
  then NODE(rank rh + 1, x, lh, rh) (* �޽���, ����� �޼� *)
  else NODE(rank lh + 1, x, rh, lh) 

and merge = function
  | (EMPTY, h) -> h
  | (h, EMPTY) -> h
  | (lh, rh) ->
    if (findMin lh) > (findMin rh)
    then shake ((findMin rh), (deleteMin rh), lh)
    else shake ((findMin lh), rh, (deleteMin lh)) (* heap Ư�� *)

and insert = function (x,h) ->
  merge(h, NODE(0,x,EMPTY,EMPTY))

and findMin = function
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,_,_) -> x

and deleteMin = function
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,lh,rh) -> merge(lh,rh)
