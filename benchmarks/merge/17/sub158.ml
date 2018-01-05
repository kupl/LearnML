let rec merge ((ll : int list), (rl : int list)) =
    match ll with
    | [] -> rl
    | lh :: lt ->
            match rl with
            | [] -> ll
            | rh :: rt ->
                if lh > rh then (lh :: merge (lt, rl))
                else            (rh :: merge (rt, ll))
