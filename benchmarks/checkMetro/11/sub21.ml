type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro

and name = string

let rec sub (l,a) =
match l with
[] -> []
| hd::tl -> if hd = a then sub(tl,a) else hd::sub(tl,a)

let rec concat (a,b) =
match a with
[] -> b
| hd::tl -> hd::concat(tl,b)

let rec listsub (m:metro) =
match m with
 STATION a -> [a]
| AREA (a,b) -> sub(listsub(b), a)
| CONNECT (a,b) -> concat(listsub(a),listsub(b))

let checkMetro (m:metro) =
match m with
 STATION a -> false
| AREA (a,b) -> if sub(listsub(b), a) = [] then true else false
| CONNECT (a,b) -> false





