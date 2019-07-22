type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro met=

let rec checkArea id m =
match m with
STATION n -> List.exists (fun x->x=n) id
| CONNECT (s1,s2) -> ((checkArea id s1) & (checkArea id s2))
| AREA (id2,m) -> checkArea (id2::id) m
in

match met with
AREA (id,m) -> checkArea [id] m
| _ -> false