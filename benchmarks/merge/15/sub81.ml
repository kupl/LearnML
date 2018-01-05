let merge: (int list * int list) -> int list = fun (a, b) ->
    let rec aux (a, b, result) = 
        match (a, b) with
        | ([], []) -> result
        | (e::_a, []) -> aux(_a, [], e::result)
        | ([], e::_b) -> aux([], _b, e::result)
        | (e1::_a, e2::_b) -> (if e1 > e2 then aux(_a, b, e1::result) else aux(a, _b, e2::result)) in
    List.rev(aux (a, b, []));;
