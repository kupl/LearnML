module type Queue =
    sig
        type element
        type queue
        exception EMPTY_Q
        val emptyQ: queue
        val enQ: queue * element -> queue
        val deQ: queue -> element * queue
    end
;;

module IntListQ = 
    struct
        type element = int list
        type queue = element list * element list
        exception EMPTY_Q
        let emptyQ = ([], [])
        let enQ = function
            | ((que1, que2), elem) -> (elem::que1, que2)
        let deQ = function
            ([], []) -> raise EMPTY_Q
            (*| (que1, []) -> deQ([], List.rev(que1))*)
            | (que1, []) -> (List.hd(List.rev(que1)), ([], List.tl(List.rev(que1))))
            | (que1, que2) -> (List.hd(que2), (que1, List.tl(que2))) 
    end
;;
