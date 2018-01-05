(*HW2-Exercise 6*)
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
        type queue = QUEUE of (element list) * (element list)
        exception EMPTY_Q
        let emptyQ = QUEUE([], [])
        let enQ (q, elt) =
            match q with QUEUE(left, right) -> QUEUE(elt::left, right)
        let deQ q =
            if q == emptyQ then raise EMPTY_Q
            else
                match q with
                | QUEUE(left, elt::right_rest) -> (elt, QUEUE(left, right_rest))
                | QUEUE(left, []) ->
                    (match List.rev left with
                     | elt::left_rest -> (elt, QUEUE([], left_rest))
                     | [] -> raise EMPTY_Q)
    end
