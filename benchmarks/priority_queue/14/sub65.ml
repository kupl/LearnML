type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
                    | NODE(r, _, _, _) -> r

let findMin = function EMPTY -> raise EmptyHeap
                      | NODE(_, x, _, _) -> x

let left = function EMPTY -> raise EmptyHeap
                    | NODE(_, _, lh, _) -> lh

let right = function EMPTY -> raise EmptyHeap
                    | NODE(_, _, _, rh) -> rh

let shake = function (x, lh, rh) ->
  if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh)

let rec merge = function (lh, rh) ->
  if lh = EMPTY
    then rh
    else
      if rh = EMPTY
        then lh
        else
          if (findMin lh) < (findMin rh)
            then shake((findMin lh), (left lh), (merge((right lh), rh)))
            else shake((findMin rh), (left rh), (merge((right rh), lh)))


let insert = function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))

let deleteMin = function EMPTY -> raise EmptyHeap
                        | NODE(_, x, lh, rh) -> merge(lh, rh)
