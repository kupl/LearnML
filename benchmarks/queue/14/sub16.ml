(*
 * 컴퓨터공학부 2009-11690 김찬민
 * Homework 2 Exercise 5  *)

module type Queue =
sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
end

module IntListQ =
struct
    type element = int list
    type queue = (element list) * (element list)
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ (((ql, qr), elt) : (queue * element)) : queue =
        (elt :: ql, qr)
    let rec deQ (q : queue) : (element * queue) =
        match q with
        | ([], []) -> raise EMPTY_Q
        | (ql, []) -> deQ ([], List.rev ql)
        | (ql, front :: rest) -> (front, (ql, rest))
end

