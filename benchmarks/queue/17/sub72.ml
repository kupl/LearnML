module type Queue =
    sig
        type element
        type queue
        exception EMPTY_Q
        val emptyQ: queue
        val enQ: queue * element -> queue
        val deQ: queue -> element * queu
    end

module IntListQ =
    struct
        type element = int list
        type queue = int list list * int list list
        exception EMPTY_Q
        let emptyQ = ([], [])
        let enQ(q,e)=
            match q with
            |(l, r) -> (e::l, r)
        let deQ q = 
            match q with
            |([], []) -> raise EMPTY_Q
            |(a, []) -> (deq ([], (List.rev a)))
            |(a, h::t) -> (h, (a, t))
    end
