let rec merge =
    fun (f, s) ->
        match (f, s) with
        | (_, []) -> f
        | ([], _) -> s
        | (h_f::t_f, h_s::t_s) ->
                if h_f > h_s
                then h_f::(merge (t_f, s))
                else h_s::(merge (f, t_s))
