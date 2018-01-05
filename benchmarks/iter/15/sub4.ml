let iter =
    fun (n, f) ->
        fun y ->
            let rec recursion =
                fun (k, x) ->
                    if k < 1 then x
                    else recursion(k-1, f x)
            in
            recursion(n, y)
