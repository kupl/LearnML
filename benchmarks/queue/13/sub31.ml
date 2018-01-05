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
        let emptyQ = ([], [])
        let enQ = function (queue, element) ->
            match (queue, element) with
            | ((list_1, list_2), x) -> (x::list_1, list_2)
        let rec deQ = function queue ->
            match queue with
            | ([], []) -> raise EMPTY_Q
            | (list_1, []) -> deQ ([], List.rev list_1)
            | (list_1, list_2) -> (List.hd list_2, (list_1, List.tl list_2))
    end

