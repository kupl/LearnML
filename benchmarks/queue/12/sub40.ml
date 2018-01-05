 module type Queue = sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue 
    val deQ: queue -> element * queue
end

module IntListQ (* : Queue with type element = int list *) = 
struct
    type element = int list
    type queue = element list * element list
    exception EMPTY_Q

    let emptyQ = (([], []) : queue)

    let enQ ((q: queue), (e : element)) = match q with
        | (q1, q2) -> ((e::q1, q2) : queue)
    let rec deQ (q : queue) = match q with
    | ([], []) -> raise EMPTY_Q
    | ((q1 : element list), (q2 : element list)) -> 
        (match q2 with
            | [] -> deQ ([], List.rev q1)
            | hd::tl -> (hd, ((q1, tl) : queue)))
end


(* TEST SET *)
(*
module ValidIntListQ = (IntListQ: Queue)
let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x,restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])
let e = IntListQ.emptyQ
let a = IntListQ.enQ (e, [1])
let b = IntListQ.enQ (a, [1; 2])
let c = IntListQ.enQ (b, [1; 2; 3])
let (el, restC) = IntListQ.deQ c
let (el1, restC1) = IntListQ.deQ restC
let (el2, restC2) = IntListQ.deQ restC1
let d = IntListQ.enQ (restC2, [1; 2; 3])
*)