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

        let emptyQ = ([], [])
        let rec enQ (q, elem) = (elem :: (fst q), snd q)

        let rec deQ q = match q with
            | ([], []) -> raise EMPTY_Q
            | (f, h::t) -> (h, (f, t))
            | (f, []) -> 
                let rec rev (l, r) = match l with
                    | [] -> r
                    | h::t -> rev(t, h::r)
                in
                deQ ([], rev(f, []))
                
    end
