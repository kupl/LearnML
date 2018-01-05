type heap = 
    EMPTY 
  | NODE of rank * value * heap * heap 
and rank = int (* EMPTY : -1
                  LEAF : 0 *)
and value = int

exception EmptyHeap

let rank : heap -> int = function 
    EMPTY -> -1 
  | NODE (r, _, _, _) -> r

let shake : value * heap * heap -> heap = 
  function (x, lh, rh) ->
    if (rank lh) >= (rank rh)
      then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh)

let rec merge : heap * heap -> heap =
  fun (h1, h2) ->
    match (h1, h2) with
    | (EMPTY, _) -> h2
    | (_, EMPTY) -> h1
    | NODE (r1, v1, h11, h12), NODE (r2, v2, h21, h22) ->
      if v1 < v2 then 
        let newHeap = merge (h12, h2) in
        shake (v1, h11, newHeap)
      else
        let newHeap = merge (h22, h1) in
        shake (v2, h21, newHeap)



let insert : value * heap -> heap =
  function (x, h) -> 
    merge(h, NODE(0, x, EMPTY, EMPTY))

let findMin : heap -> value = function 
    EMPTY -> raise EmptyHeap
  | NODE(_, x, _, _) -> x

let deleteMin : heap -> heap = function 
    EMPTY -> raise EmptyHeap
  | NODE(_, x, lh, rh) -> merge(lh,rh)




