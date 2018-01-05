(* PL HW2-5, "Queue = 2 Stacks"
   2007-11738
   알렉산더 *)

module type Queue =
    sig
        type element
        type queue
        exception EMPTY_Q
        val emptyQ: queue
        val enQ: queue * element -> queue
        val deQ: queue -> element * queue
    end

(* Int List Queue *)
module IntListQ : Queue with type element = int list =
    struct
        type element = int list
        type queue = int list list * int list list
        exception EMPTY_Q
        let emptyQ = ([], [])
        let enQ (q, e) =
            match q with
                (l1, l2) -> (e::l1, l2)
        let rec deQ q =
            match q with
                ([], []) -> raise EMPTY_Q
              | (l1, []) -> deQ ([], List.rev l1)
              | (l1, h::t) -> (h, (l1, t))
    end


(* TEST *)
(*
let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x, restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])
*)
