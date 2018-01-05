type metro = STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro
and name = string

let checkMetro : metro -> bool = fun m ->
    let rec sub_check : metro -> string list -> bool = fun m l ->
        match m with
        | STATION n -> List.exists (fun x -> x = n) l
        | AREA (n, m) -> sub_check m (n::l)
        | CONNECT (m1, m2) -> (sub_check m1 l) && (sub_check m2 l)
    in  
    sub_check m []

