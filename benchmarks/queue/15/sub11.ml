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
        let enQ: queue * element -> queue = fun(q, e) ->
            (e::fst(q), snd(q)) 
        let rec deQ: queue -> element * queue = fun(q) ->
            match (fst(q), snd(q)) with
            | ([], []) -> raise EMPTY_Q
            | (l, []) -> deQ([], List.rev l)
            | (l, r) -> (List.hd(r), (l, List.tl(r)))
    end


(*
module ValidIntListQ = (IntListQ : Queue)

let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x,restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])

*)


