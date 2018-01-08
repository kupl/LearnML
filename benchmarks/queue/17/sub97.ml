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
        type queue = (int list list) * (int list list)
        exception EMPTY_Q
        let emptyQ = ([], [])
        let enQ (q, x) = (x::(fst q), (snd q))
        let deQ q = match q with
                    |[], [] -> raise(EMPTY_Q)
                    |a, []-> ((List.hd (List.rev a)), ([], List.tl (List.rev a)))
                    |a, h::t -> (h, (a, t)) 
    end