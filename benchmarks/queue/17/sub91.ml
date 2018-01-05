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
        let emptyQ : queue = ([], [])
        let enQ ((q : queue), (e : element)) : queue =
            match q with
            | ([], []) -> (e::[], [])
            | (q1, q2) -> (e::q1, q2)
        let deQ (q : queue) : (element * queue) =
            match q with
            | ([], []) -> raise (EMPTY_Q)
            | (q1, e::q2) -> (e, (q1, q2))
            | (q1, []) -> let q1m = List.rev q1 in
                          match q1m with
                          | [] -> raise (EMPTY_Q)
                          | e::q1mm -> (e, ([], q1mm))
    end