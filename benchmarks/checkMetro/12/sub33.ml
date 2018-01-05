type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkMetro mtr=

let rec cM mtr lst=
match mtr with
| STATION a -> List.mem a lst
| AREA (a, m) -> (cM m (a::lst))
| CONNECT (a, b) -> (cM a lst) && (cM b lst)
in

(cM mtr [])
;;