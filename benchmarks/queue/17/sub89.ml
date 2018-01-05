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
        type queue = element list
        exception EMPTY_Q
        let emptyQ : queue = []
        let enQ ((q : queue), (e : element)) : queue =
            match q with
            | [] -> e::[]
            | _ -> e::q
        let deQ (q : queue) : (element * queue) =
            let qm = List.rev q in
            match qm with
            | [] -> raise (EMPTY_Q)
            | e::qmm -> (e, List.rev qmm)
    end