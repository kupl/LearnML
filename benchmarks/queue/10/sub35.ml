(* 4190.310 Programming Language 				*
 * Homework #2 - Exercise 4 (Queue = 2 Stacks)	*
 * 2008-11744 Jongwook Choi 					*)

module type Queue =
sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
end

module IntListQ (* : Queue *) =
struct
    type element = int list
    type queue = element list * element list
    exception EMPTY_Q

    let emptyQ = ([], [])

    let enQ ((ql, qr), item) =
        (item::ql, qr)

    let deQ (ql, qr) = match (ql, qr) with
        | (_, h::t) -> (h, (ql, t))
        | (_, []) -> let nqr = List.rev_append ql qr in
            match nqr with
              [] -> raise EMPTY_Q
            | h::t -> (h, ([], t))
end

