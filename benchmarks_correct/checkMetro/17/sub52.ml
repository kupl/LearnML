type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro: metro -> bool = fun met ->
    let rec saveCheck : metro * name list -> bool = 
        fun (m, l) -> match m with
        | STATION x -> List.mem x l
        | AREA (n, m1) ->  saveCheck(m1, n::l)
        | CONNECT (m1, m2) -> saveCheck(m1, l) && saveCheck(m2, l)
    in saveCheck(met, [])
