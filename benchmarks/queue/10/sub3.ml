(* CSE/ 2004-11920 / Yeseong Kim/ Prob 4*)
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
        type queue = (element * element)
        exception EMPTY_Q
        let emptyQ = ([], [])
        let enQ (que, item) =
                match que with
                        (l,r) -> (item::l, r)
        let deQ que =
                let rec sub_deQ lQue rQue =
                        match rQue with
                                h::t -> (h, (lQue, t))
                        |       [] -> sub_deQ [] (List.rev lQue)
                in
                match que with (* Check Queue is not empty *)
                        ([], []) -> raise EMPTY_Q
                |       (l, r) -> sub_deQ l r
end

