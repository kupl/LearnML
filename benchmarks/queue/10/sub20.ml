module type Queue =
    sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ : queue
    val enQ : queue * element -> queue
    val deQ : queue -> element * queue
end

module IntListQ =
    struct
    type element = int list
    type queue = (element list * element list)
    exception EMPTY_Q
    let emptyQ = ([],[])
    let enQ (q, e) =
        match q with
            (x,y) -> ([e]@x, y)
    let rec deQ q =
        let rec move l1 l2=
        match l1 with
        [] -> l2
        | (h::t) -> (move t l2) @ [h] in

        match q with
            (x,(h::t)) -> (h, (x,t))
            | ((h::t),[]) -> (deQ (([],(move (h::t) []))))
            | ([],[]) -> raise EMPTY_Q
    end

