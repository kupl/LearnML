type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetro_aux: metro -> name list -> bool =
  fun m nl ->
    match m with
    | STATION n -> List.exists (fun n1 -> (compare n n1 = 0)) nl
    | AREA (n, m1) -> checkMetro_aux m1 (n::nl)
    | CONNECT (m1, m2) -> (checkMetro_aux m1 nl) && (checkMetro_aux m2 nl)

let checkMetro: metro -> bool =
  fun m ->
    checkMetro_aux m []
