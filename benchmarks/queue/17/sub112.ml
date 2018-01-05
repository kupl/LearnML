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
        type queue = int list list * int list list
        exception EMPTY_Q
        let emptyQ = ([], [])
        let enQ (q, e) : queue = 
            match q with
            | ([], []) -> ([], e::[])
            | (a, b) -> (e::a, b)
        let deQ q : element * queue = 
            match q with
            | (a, b::[]) -> (b, ([], List.rev a))
            | (a, b::c) -> (b, (a, c))
            | _ -> raise EMPTY_Q
    end
