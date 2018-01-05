(* 4190,310 Programming Language (Fall 2014)
 * Homework 2 - Exercise 5
 * CSE / 2012-13456 / Gao, Chengbin *)

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
        type queue = element list * element list
        exception EMPTY_Q
        let emptyQ = ([], [])
        let enQ (q, e) = 
            match q with
            | (a, b) -> (e::a, b)
        let rec deQ q =
            match q with
            | ([], []) -> raise EMPTY_Q
            | (a, []) -> deQ ([], List.rev a)
            | (a, hd::tl) -> (hd, (a, tl))
    end

    (* TODO : test *)
        ;; module ValidIntListQ = (IntListQ: Queue)
