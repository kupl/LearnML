type name = string
type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro

let checkMetro : metro -> bool = fun m ->
  let rec findname : metro -> name list -> bool = fun n l ->
    match n with
    | STATION o -> List.mem o l
    | AREA (o, p) -> findname p (o::l)
    | CONNECT (p, q) -> (findname p l) && (findname q l)
  in
  findname m []
