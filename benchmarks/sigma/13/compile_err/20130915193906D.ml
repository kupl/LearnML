type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec licheck s1 lst1 =
  match lst1 with
  | head::tail -> (s1 = head) || (licheck s1 tail)
  | [] -> false

let rec sicheck m lst =
  match m with
  | AREA(s,m1) -> (sicheck m1 (s::lst))
  | CONNECT(m1,m2) -> (sicheck m1 lst) && (sicheck m2 lst)
  | STATION s -> (licheck s lst)

let rec checkMetro mtr =
 sicheck mtr []