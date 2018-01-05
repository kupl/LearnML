(* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 2, Exercise 6 *)

module type Queue =
    sig
        type element
        type queue
        exception EMPTY_Q
        val emptyQ : queue
        val enQ : queue * element -> queue
        val deQ : queue -> element * queue
    end

module IntListQ =
    struct
        type element = int list
        type queue = element list * element list
        exception EMPTY_Q
        let emptyQ = ([], [])
        let enQ ((l, r), e) = (e::l, r)
        let deQ (l, r) =
            let deQueue (l, r) =
                match r with
                | e::rest -> (e, (l, rest))
                | [] -> raise EMPTY_Q
            in
            match (l, r) with
            | (l, e::rest) -> (deQueue (l, e::rest))
            | (l, []) -> deQueue ([], (List.rev l))
    end



(* module ValidIntListQ = (IntListQ : Queue) *)
