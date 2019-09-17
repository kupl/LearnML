type lambda = V of var
| P of var * lambda
| C of lambda * lambda 

and var = string

let rec __check (m, l) =
  match m with 
  V a -> if (List.exists (fun x -> (x = a)) l) then true else false
  |P (_var, _lambda) -> __check (_lambda, _var::l)
  |C (m1, m2) -> __check (m1, l) && __check (m2, l)

let check m =
  __check (m, [])
