let rec merge: int list * int list -> int list = fun (a, b) ->
    match (a, b) with
    | ([], b) -> b
    | (a, []) -> a
    | (a_head :: a_tail, b_head :: b_tail) ->
            if (a_head > b_head)
                then a_head :: (merge (a_tail, b))
                else b_head :: (merge (a, b_tail))
