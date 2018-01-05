module type Queue =
sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
end

module IntListQ = struct
        type element = int
        type queue = int list * int list
        exception EMPTY_Q
        let emptyQ = ([], [])
        let rec enQ (q, elem) =
            match q with (a, b) ->
                ((elem::a), b)
        let rec deQ (a, b) =
            begin match b with
            | (elem :: rest) -> (elem, (a, rest))
            | [] -> begin match List.rev(a) with
                         | [] -> raise EMPTY_Q
                         | elem :: rest -> (elem, ([], rest))
                    end
            end
    end


