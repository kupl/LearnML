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
        type queue = element * element
        exception EMPTY_Q
        let emptyQ = ([], [])
        let enQ(q, elt) =
            let (l, r) = q
            in
            ((elt@l), r)
        let deQ q =
            match q with
            | ([], []) -> raise EMPTY_Q
            | (l, []) ->
                    let r = List.rev l in ([List.hd r], ([], List.tl r))
            | (l, h::t) -> ([h], (l, t))
    end
