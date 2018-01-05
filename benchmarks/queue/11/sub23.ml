module type Queue =
    sig
        type element
        type queue
        exception EMPTY_Q
        val emptyQ: queue
        val enQ: queue * element -> queue
        val deQ: queue -> element * queue
    end




module IntListQ : Queue with type element = int list =
    struct
        type element = int list
        type queue = int list list * int list list
        exception EMPTY_Q
        let emptyQ = ([],[])
        let enQ (queue, e) =
            match queue with
            | (hd,tl) -> (e::hd,tl)

        let deQ (queue) = 
            match queue with
            | ([], []) -> raise EMPTY_Q
            | (fl, []) -> (match List.rev fl with
            | [] -> raise EMPTY_Q
            | hd::tl -> (hd,  ([],tl))
            )
            | (x, hd::tl) -> (hd, (x, tl))
    end





