type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro

and name = string

let rec inlist a l = 
match l with
| [] -> false
| h::t -> if a = h then true
	else (inlist a t)

let rec checkm l m =
match m with
| STATION a -> (inlist a l)
| AREA (a, b) -> (checkm (a::l) b)
| CONNECT (a, b) -> (checkm l a) && (checkm l b)

let rec checkMetro m = 
(checkm [] m)
