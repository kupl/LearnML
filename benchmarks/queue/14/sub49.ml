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
    let emptyQ = ([],[])
    let enQ: queue * element -> queue =
        fun (q,e) ->
            ((e::(fst q)), (snd q))
    let deQ: queue -> element * queue =
        fun q ->
            if q = emptyQ then raise EMPTY_Q
            else
                let llist = (fst q) in
                let rlist = (snd q) in
                if rlist = [] then
                    let lrev = List.rev llist in
                        (List.hd lrev, ([], List.tl lrev))
                else
                    (List.hd rlist, (llist, List.tl rlist))
end

module ValidIntListQ = (IntListQ: Queue)
