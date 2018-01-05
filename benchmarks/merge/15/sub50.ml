let rec merge((a: int list), (b: int list)) =
        match (a,b) with
        | (ha::ta, hb::tb) ->
                        if(ha < hb) then hb :: merge(a, tb)
                        else ha :: merge(ta, b)
        | (ha::ta, []) -> a
        | ([], _) -> b
;;

