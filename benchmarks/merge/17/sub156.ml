let rec merge : int list * int list -> int list = fun p ->
    match p with
    |([], s)  -> s
    |(f, [])  -> f
    |(f, s) ->
        if(List.hd f > List.hd s) then List.hd f :: merge(s, List.tl f)
        else List.hd s :: merge (f, List.tl s)