(* 컴퓨터공학과/2017-34165/김성국/2-4 *)
type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro m =
  let rec helper m lst =
    match m with
    | STATION n -> List.mem n lst
    | AREA(n, m1) -> helper m1 (n::lst)
    | CONNECT(m1, m2) -> (helper m1 lst) && (helper m2 lst)
  in
  helper m []
