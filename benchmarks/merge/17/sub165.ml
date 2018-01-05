let rec merge : int list * int list -> int list = fun (p1,p2) ->
        match (p1,p2) with
        |([],[])->[]
        |([],p2)->p2
        |(p1,[])->p1
        |(n1::l1,n2::l2)->if n1>n2 then n1::merge (l1,p2) else n2::merge (p1,l2)
