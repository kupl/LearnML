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
                type queue = Queue of element list * element list
                exception EMPTY_Q
                (* val emptyQ: queue *)
                let emptyQ = Queue([], [])
                (* val enQ: queue * element -> queue *)
                let enQ (q, e) = match q with Queue(l1, l2) -> Queue(e::l1, l2) 
                (* val deQ: queue -> element * queue *)
                let rec deQ q =
                        match q with
                        | Queue(l, h::t) -> (h, Queue(l, t))
                        | Queue(h::t, []) -> (deQ (Queue([], List.rev (h::t))))  
                        | Queue([], []) -> raise (EMPTY_Q)
        end
