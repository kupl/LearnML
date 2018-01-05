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
        let emptyQ = ([],[])
        let enQ (a,b) =
            match a with
                |(x,y) -> ([b]@x,y)
        let deQ a =
            match a with
                |(x,y) -> if y = [] then (if x = [] then raise EMPTY_Q else(List.hd (List.rev x),([],List.tl (List.rev x))))
                    else (List.hd y,(x,List.tl y))
    end
