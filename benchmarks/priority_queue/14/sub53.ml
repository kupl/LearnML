(* 4190,310 Programming Language (Fall 2014)
 * Homework 2 - Exercise 2
 * CSE / 2012-13456 / Gao, Chengbin *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
                  | NODE(r, _, _, _) -> r

let shake = function (x, lh, rh) ->
    if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh)

let rec merge (lh, rh) =
    match (lh, rh) with
    | (EMPTY, rh) -> rh
    | (lh, EMPTY) -> lh
    | (NODE(_,lv,ll,lr), NODE(_,rv,rl,rr)) ->
            if lv <= rv
            then shake(lv, merge(ll, lr), rh)
            else shake(rv, lh, merge(rl, rr))

let insert = function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
                     | NODE(_, x, _, _) -> x

let deleteMin = function EMPTY -> raise EmptyHeap
                       | NODE(_, x, lh, rh) -> merge(lh, rh)
(* TODO: test this over *)
