type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro (met: metro): bool = 
    let rec checkMetro' (met: metro) (lst: string list) = match met with
        | STATION(n) -> List.exists (fun x -> x = n) lst
        | AREA(n, met) -> checkMetro' met (n :: lst)
        | CONNECT(met1, met2) -> 
                (checkMetro' met1 lst) && (checkMetro' met2 lst)
    in
    checkMetro' met []
