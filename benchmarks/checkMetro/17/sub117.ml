type metro =
    | STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro
and name = string

let rec checkMetro: metro -> bool = fun (metro) ->
    let rec checkArea: string list * metro -> bool = fun (nl, m) ->
        match m with
        | STATION s ->
                let rec isExist: string list * string -> bool = fun (str_list, str) ->
                    match str_list with
                    | [] -> false
                    | e::e_list -> if e = str then true else isExist (e_list, str)
                in
                isExist (nl, s)
        | AREA (a, b) -> checkArea (a::nl, b)
        | CONNECT (a, b) -> checkArea (nl, a) && checkArea (nl, b)
    in
    match metro with
    | STATION n -> false
    | AREA (n, m) -> checkArea ([n], m)
    | CONNECT (m1, m2) -> (checkMetro m1) && (checkMetro m2)
