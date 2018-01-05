module type Queue =
sig
type element = int list
type queue = int list list * int list list
exception EMPTY_Q
val emptyQ: queue
val enQ: queue * element -> queue
val deQ: queue -> element * queue
end

module IntListQ : Queue =
struct
type element = int list
type queue = (int list list) * (int list list)
exception EMPTY_Q
let emptyQ = ([], [])
let enQ (q, e) =
        match q with
        (l, r) -> (e::l, r)
let rec deQ q = 
        match q with
        ([], []) -> raise (EMPTY_Q)
        |(l, e::r) -> (e, (l,r))
        |(a::b, []) -> deQ ([], List.rev (a::b))
end
