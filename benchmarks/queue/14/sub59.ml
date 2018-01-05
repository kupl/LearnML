module type Queue =
    sig
        type element
        type queue
        exception EMPTY_Q
        val emptyQ : queue
        val enQ : queue * element -> queue
        val deQ : queue -> element * queue
    end;;

module IntListQ =
    struct
        type element = int list
        type queue = element list * element list
        exception EMPTY_Q
        let emptyQ = ([],[])
        let enQ (q,e) = match q with
            (a,b) -> 
                let a_new = e::a in
                (a_new,b)

        let rec deQ q = match q with
            ([],[]) -> raise EMPTY_Q
            | (a,hd::tl) -> (hd,(a,tl))
            | (a,[]) -> deQ([],(List.rev_append a []))
    end
