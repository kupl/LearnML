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
        type element = int
        type queue = int list * int list
        exception EMPTY_Q
        let emptyQ = ([], [])
        let enQ (q, x) =
            let (l_list, r_list) = q in (x::l_list, r_list)
        let deQ q = 
            match q with
                | ([], []) -> raise EMPTY_Q
                | (l_list, []) -> (
                    let l_list_rev = List.rev l_list in
                    ((List.hd l_list_rev), ([], (List.tl l_list_rev)))
                )
                | (l_list, x::r_list_tl) -> (x, (l_list, r_list_tl))

    end
