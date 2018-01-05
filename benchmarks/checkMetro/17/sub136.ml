type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string


let checkMetro : metro -> bool = function m ->
  let rec helper alist m  =
    match m with
    | STATION s -> List.mem s alist
    | AREA (n, m') -> helper (n::alist) m'
    | CONNECT (x, y) -> helper alist x && helper alist y
  in
  helper [] m
