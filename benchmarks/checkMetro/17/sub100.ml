type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
  and name = string

let rec solver (ln,mt) : bool =
  match mt with
  | STATION(n) -> List.mem n ln
  | AREA(n,m) -> solver(ln@[n],m)
  | CONNECT(m1,m2) -> solver(ln,m1)&&solver(ln,m2)
let checkMetro mt : bool =
  solver([],mt)
