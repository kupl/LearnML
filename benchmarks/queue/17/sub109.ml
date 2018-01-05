(* 2015-1478 Giyeon Kim HW 2 *)

(* Exercise 6 *)
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
        let enQ: queue * element -> queue = fun (q, e) ->
            (e::(fst q), snd q)
        let rec deQ: queue -> (element * queue) = fun q ->
            match q with
            |([], []) -> raise EMPTY_Q
            |(llist, hd::tl) -> (hd, (llist, tl))
            |(llist, []) -> deQ([], List.rev llist)
    end

