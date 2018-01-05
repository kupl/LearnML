let rec merge (a, b) =
    match (a, b) with
    | (_a, []) -> _a
    | ([], _b) -> _b
    | (e1::_a, e2::_b) -> (if e1 > e2 then (e1::merge(_a, (e2::_b))) else (e2::merge((e1::_a), _b)));;
