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
let enQ = fun ((l1,l2),e) -> (e::l1,l2)
let deQ = fun q ->
                (match q with
                        |([],[]) -> raise EMPTY_Q
                        |([],hr::tr) -> (hr,([],tr))
                        |(ql,hr::tr) -> (hr,(ql,tr))
                        |(ql,[]) -> (match (List.rev ql) with
                                        [] -> raise EMPTY_Q
                                        |(hl::tl) -> (hl,([],tl))))
end