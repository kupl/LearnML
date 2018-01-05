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
    type queue =  int list * int list
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ: queue * element -> queue = fun x ->
        match x with
        | (q, []) -> q
        | ((a, b), elem) -> (elem @ a, b)

    let rec deQ: queue -> element * queue = fun x ->
        match x with
        | emptyQ -> raise EMPTY_Q
        | (l, []) -> deQ ([], List.rev l)
        | (l, hd::tl) -> ([hd], (l, tl))
end
