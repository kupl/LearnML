
let rec merge : int list * int list -> int list =
  fun (l, r) ->
    match (l, r) with
    | (_, []) -> l
    | ([], _) -> r
    | (h_l::t_l, h_r::t_r) ->
        if h_l > h_r then h_l::(merge (t_l, r)) else h_r::(merge (l, t_r))
