type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkMetro : metro -> bool = function x -> (
  let rec checkMetro_temp x (l : name list) : bool = match x with
  | STATION n -> List.exists (fun x -> x = n) l
  | AREA (n, m) -> checkMetro_temp m (n::l)
  | CONNECT (m1, m2) -> if (checkMetro_temp m1 l) then (checkMetro_temp m2 l) else false
  in checkMetro_temp x []
)
