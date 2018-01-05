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
        type queue = int list list * int list list
        exception EMPTY_Q
        let emptyQ = ([],[])
        let enQ (a,e) =
                match a with
                | (c,d) -> (e::c,d)
        let rec deQ a =
                match a with
                | ([],[]) -> raise EMPTY_Q
                | (c,d) -> match d with
                        | [] -> (deQ ([],List.rev(c)))
                        | hd::tl -> (hd,(c,tl))
end

