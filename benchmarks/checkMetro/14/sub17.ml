(* hw2-1 *)

type metro =
    STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let checkMetro m =
  let rec checkMetroHelper g m =
    match m with
    | STATION s -> List.mem s g
    | AREA (a, n) -> checkMetroHelper (a::g) n
    | CONNECT (n1, n2) -> (checkMetroHelper g n1) && (checkMetroHelper g n2)
  in
  checkMetroHelper [] m
