(* 4190.310 Programming Languages - Daegeun Lee <elnn@elnn.kr> *)

module type Queue =
sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
end

module IntListQ : Queue with type element = int list =
struct
    type element = int list
    and queue = (element list) * (element list)
    exception EMPTY_Q

    let emptyQ = ([],[]) 

    let enQ = fun (q,e) ->
        match q with
        | (q1,q2) -> (e::q1,q2)

    let rec deQ = fun q ->
        match q with
        | ([],[]) -> raise EMPTY_Q
        | (q1,[]) -> deQ([],List.rev(q1))
        | (q1,hd::tl) -> (hd,(q1,tl))
end
