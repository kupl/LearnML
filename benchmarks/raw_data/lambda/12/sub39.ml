type lambda = V of var
| P of var * lambda
| C of lambda * lambda 

and var = string

let rec check2 (m, l) =
  match m with 
  V a -> if (List.exists (fun x -> (x = a)) l) then true else false
  |P (_var, _lambda) -> check2 (_lambda, _var::l)
  |C (m1, m2) -> check2 (m1, l) && check2 (m2, l)

let check m =
  check2 (m, [])
