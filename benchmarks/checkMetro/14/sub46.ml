(* hw 2-1 *)
(* 2012-11269 DongJae Lim *)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec cM ((m : metro), (ml : name list)) : bool =
  match m with
  | STATION (n0) -> (List.mem n0 ml)
  | AREA (n0, m0) -> (cM (m0, ml @ [n0]))
  | CONNECT (m0, m1) -> (cM (m0, ml)) && (cM (m1, ml))

let checkMetro (m : metro) : bool =
  cM (m, [])
