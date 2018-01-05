
let rec merge (l0, l1) =
  match (l0, l1) with
    | (_, []) -> l0
    | ([], _) -> l1
    | (x :: l0_, y :: l1_) ->
        if x < y then y :: (merge (l0, l1_))
        else x :: (merge (l0_, l1))
