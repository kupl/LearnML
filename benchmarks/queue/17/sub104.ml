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
        type queue = element list*element list
        exception EMPTY_Q
        let emptyQ : queue = ([],[])
        let enQ ((q:queue),(e:element)) : queue =
            match q with
            | (r, l) -> (e::r, l)
        let deQ (q:queue) : element*queue =
            match q with
            | (r, l) -> (
                match l with
                | [] -> (
                    match List.rev r with
                    | [] -> raise EMPTY_Q
                    | hd::tl -> (hd,([], tl))
                )
                | hd::tl -> (hd,(r,tl))
            )
    end


