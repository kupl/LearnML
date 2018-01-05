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
        let enQ : queue * element -> queue = fun a ->
            match a with
            |(c,d) -> (d::fst c,snd c) 
        let deQ : queue -> element * queue = fun c -> 
            match snd c with 
            |hd::tl -> (hd,(fst c, tl))
            |[] -> 
                match fst c with
                |[] -> raise EMPTY_Q
                |a -> 
                 let d : element list = List.rev a in
                 (List.hd d,([], snd c@(List.tl d)) )
    end