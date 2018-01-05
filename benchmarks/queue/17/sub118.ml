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
        type queue = element list * element list
        exception EMPTY_Q
        let emptyQ = ([], [])
        let enQ ((left, right), e) = (e::left, right)
        let deQ q =
            match q with
            | ([], []) -> raise EMPTY_Q
            | (left, []) -> (
                    let l = List.rev left
                    in
                    match l with
                    | [] -> raise EMPTY_Q
                    | hd::tl -> (hd, ([], tl))
            )
            | (left, r::right) -> (r, (left, right))
    end
