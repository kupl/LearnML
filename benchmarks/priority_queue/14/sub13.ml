type heap = EMPTY
            | NODE of rank * value * heap * heap
  and rank = int
  and value = int

exception EmptyHeap

let rank: heap -> rank =
  fun h ->
    match h with
    | EMPTY -> -1
    | NODE (r, _, _, _) -> r

let shake: value * heap * heap -> heap =
  fun (x, lh, rh) ->
    if (rank lh) >= (rank rh)
      then NODE (rank rh + 1, x, lh, rh)
      else NODE (rank lh + 1, x, rh, lh)

let rec merge: heap * heap -> heap =
  fun (lh, rh) ->
    match (lh, rh) with
    | (EMPTY, _) -> rh
    | (_, EMPTY) -> lh
    | (NODE (lr, lx, llh, lrh), NODE (rr, rx, rlh, rrh))->
        if lx < rx then
          let newleft = llh in
          let newright = merge (rh, lrh) in
          shake (lx, newleft, newright)
        else
          let newleft = rlh in
          let newright = merge (lh, rrh) in
          shake (rx, newleft, newright)

let insert: value * heap -> heap =
  fun (x, h) ->
    merge (h, NODE (0, x, EMPTY, EMPTY))

let findMin: heap -> value =
  fun h ->
    match h with
    | EMPTY -> raise EmptyHeap
    | NODE (_, x, _, _) -> x

let deleteMin: heap -> heap =
  fun h ->
    match h with
    | EMPTY -> raise EmptyHeap
    | NODE (_, x, lh, rh) -> merge (lh, rh)
