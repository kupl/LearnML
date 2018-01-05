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
        let emptyQ =
            ([], [])

        let enQ ((q: queue), (elem: element)) : queue =
            match q with
            | (a, b) -> (elem :: a, b)

        let rec deQ (q: queue) : (element * queue) =
            match q with
            | ([], []) -> raise EMPTY_Q
            | (a, []) -> deQ ([], (List.rev a))
            | (a, h::t) -> (h, (a, t))
    end
