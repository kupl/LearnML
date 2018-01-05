let merge : int list * int list -> int list = fun (list1, list2) ->
        let rec aux l = function
        | ([], []) -> l
        | ([], element::t) -> aux (l @ [element]) ([], t)
        | (element::t, []) -> aux (l @ [element]) (t, [])
        | (elem_1::t1, elem_2::t2) -> if elem_1 > elem_2 then aux (l @ [elem_1]) (t1, elem_2::t2) else aux (l @ [elem_2]) (elem_1::t1, t2)
        in aux [] (list1, list2);;
