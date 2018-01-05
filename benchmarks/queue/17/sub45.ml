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
        type queue = (int list) list * (int list) list
        exception EMPTY_Q
        let emptyQ = ([],[])
        let enQ ((q : queue), (e : element)) : queue = 
            match q with
                (a, b) -> (e :: a, b)

        let deQ (q : queue) : element * queue = 
            match q with
                ([],[]) -> raise EMPTY_Q
              | (a, x::b) -> (x, (a, b))
              | (a, []) -> (match List.rev a with
                                x::b -> (x, ([], b))
                              | _ -> raise EMPTY_Q)
    end
