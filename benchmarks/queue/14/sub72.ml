module type Queue=
    sig
        type element
        type queue
        exception EMPTY_Q
        val emptyQ: queue
        val enQ : queue * element -> queue
        val deQ : queue -> element * queue
    end

module IntListQ =
    struct
        type element = int list
        type queue = element list * element list
        exception EMPTY_Q
        let emptyQ:queue = ([],[])
        let enQ (((l1,l2):queue), (el:element)) : queue=
            (el::l1,l2)
        let rec deQ (q:queue) : element*queue =
            match q with
            | ([],[]) -> raise EMPTY_Q
            | (l1,[]) -> (deQ ([],List.rev l1))
            | (l1,el::l2) -> (el,(l1,l2))
    end

