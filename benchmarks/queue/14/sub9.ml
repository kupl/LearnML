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
        type queue = Node of element list * element list
        exception EMPTY_Q
        let emptyQ = Node ([], [])

        let rec reverse queue =
            match queue with
                | Node ([], list2) -> Node ([], list2)
                | Node (element::list1, list2) -> (reverse (Node (list1, element::list2)));;

        let rec enQ = function ((Node (list1, list2)), element) ->
            Node (element::list1, list2)

        let rec deQ queue =
            match queue with
                | (Node ([], [])) -> raise EMPTY_Q
                | (Node (list1, [])) -> (deQ (reverse (Node (list1, []))))
                | (Node (list1, element::list2)) -> (element, (Node (list1, list2)));;

    end;;
