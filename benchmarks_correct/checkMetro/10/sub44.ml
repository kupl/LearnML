exception Error of string;;

let rec inclu l a = match l with
    [] -> false
  | hd :: tl -> if hd = a then true 
    else (inclu tl a);;

type metro = STATION of name
	     | AREA of name * metro
	     | CONNECT of metro * metro
and name = string;;

let rec checker l m = match m with 
    (STATION n) -> (inclu l n)
  | (AREA (n, m1)) -> (checker (n::l) m1)
  | (CONNECT (m1, m2)) -> (match ((checker l m1), (checker l m2)) with 
			      (true, true) -> true
			    | (_, _) -> false
			 );;
let checkMetro m = (checker [] m);;
