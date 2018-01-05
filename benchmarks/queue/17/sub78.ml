module type Queue = 
        sig
                type element
                type queue
                exception EMPTY_Q
                val emptyQ : queue
                val enQ : queue * element -> queue
                val deQ : queue -> element * queue
        end;;

module IntListQ : Queue = 
        struct
                type element = int list
                type queue = (element list) * (element list)
                exception EMPTY_Q
                let emptyQ = ([], [])
                let enQ ((q : queue), (e : element)) : queue = 
                       ( (e::(fst q)), (snd q) )
                let deQ (q : queue) : element * queue =
                        match q with
                        | ([], []) -> raise EMPTY_Q
                        | (l, []) -> (let lLast::lRev = List.rev l in
                                (lLast, ([], lRev)))
                        | (l, r::r1) -> (r, (l, r1))
        end;;

