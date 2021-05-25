type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec check2 : lambda * string list -> bool = fun (m, l) ->
  match m with
  | V a -> List.mem a l
  | P(a,m2) -> check2(m2,a::l)
  | C(m1,m2) -> check2(m1,l) && check2(m2,l)

let check : lambda -> bool = fun m ->
  check2(m, [])
