(*컴퓨터공학부/2011-11729/안진우/2-6*)

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
                let enQ ((q: queue), (e: element)) : queue =
                        match q with
                        | (left, right) -> (List.append [e] left, right) 
                let deQ (q: queue) : element * queue = 
                        match q with
                        | ([], []) -> raise (EMPTY_Q)
                        | (left, []) -> (List.hd(List.rev(left)), ([], List.tl(List.rev(left))))
                        | (left, right) -> (List.hd(right), (left, List.tl(right))) 
        end

