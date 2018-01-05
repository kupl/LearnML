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
        type queue = INTLISTQ of element list * element list
        exception EMPTY_Q
        let emptyQ = INTLISTQ([], [])
        let enQ (q, e) =
            match q with
            | INTLISTQ(a, b) -> INTLISTQ(e::a, b)
        let rec deQ q =
            match q with
            | INTLISTQ([], []) -> raise EMPTY_Q
            | INTLISTQ(a, []) -> deQ (INTLISTQ([], (List.rev a)))
            | INTLISTQ(a, e::b) -> (e, INTLISTQ(a, b))
    end
