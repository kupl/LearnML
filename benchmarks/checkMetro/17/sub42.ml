type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro (m: metro) : bool =
  let rec helper ((met: metro), (l: name list)) : bool =
    match met with
    | STATION x -> List.mem x l
    | AREA (x, y) -> helper (y, x::l)
    | CONNECT (y, z) -> helper (y, l) && helper (z, l)
  in
  helper (m, [])