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
        type queue = element list * element list
        exception EMPTY_Q
        let emptyQ: queue = ([], [])
        let enQ ((q: queue), (e: element)): queue = 
            match q with
            (left, right) -> (e::left, right)
        let rec deQ (q: queue): element * queue = 
            if (q = emptyQ) then raise (EMPTY_Q)
            else match q with
            (left, hd::tl) -> (hd, (left, tl))
            | (left, []) -> deQ ([], List.rev left)
    end
