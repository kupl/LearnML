module type Queue =
    sig
        type element
        type queue
        exception EMPTY_Q
        val emptyQ: queue
        val enQ: queue * element -> queue
        val deQ: queue -> element * queue
    end;;


module IntListQ = 
    struct
        type element = int list
        type queue = element list * element list
        exception EMPTY_Q
        let emptyQ = ([],[])
        let enQ = fun (q,e) -> (
            match q with
            (l,r) -> (e::l,r)
        )
        let deQ = fun q -> (
            match q with
            ([],[]) -> raise EMPTY_Q
            | (l,[]) -> (
                let revl = List.rev(l) in
                match revl with
                [] -> raise EMPTY_Q
                | hd::tl_of_newQr -> (hd, ([],tl_of_newQr))
            )
            | (l,hd::r) -> (
                (hd,(l,r))
            )
        )
    end
