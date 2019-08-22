type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string;;

let rec check2 m l =
match m with
| V n1 -> List.exists (fun x -> x = n1) l 
| P (n1, m1) -> check2 m1 (n1::l)
| C (m1, m2) -> check2 m1 l && check2 m2 l

let check lambda = check2 lambda []


