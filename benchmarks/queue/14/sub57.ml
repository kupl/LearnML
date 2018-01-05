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
        let enQ = function
            | ((l,r),e) -> (e::l,r)
        let deQ = function
            | ([],[]) -> raise EMPTY_Q
            | (l,e::r) -> (e,(l,r))
            | (l,[]) ->
                    let r = List.rev l in
                    ((List.hd r),([],(List.tl r)))
    end;;
