exception Error of string;;

let rec inclu l a = match l with
    [] -> false
  | hd :: tl -> if hd = a then true 
    else (inclu tl a);;

type lambda = V of var
	     | P of var * lambda
	     | C of lambda * lambda
and var = string;;

let rec checker l m = match m with 
    (V n) -> (inclu l n)
  | (P (n, m1)) -> (checker (n::l) m1)
  | (C (m1, m2)) -> (match ((checker l m1), (checker l m2)) with 
			      (true, true) -> true
			    | (_, _) -> false
			 );;
let check m = (checker [] m);;
