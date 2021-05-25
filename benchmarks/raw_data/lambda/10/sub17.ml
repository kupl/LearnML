exception Error of string
type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
                         and var = string

let check : lambda -> bool =
  fun lambda ->
    let rec check2 : lambda -> var list -> bool =
      fun lambda -> fun env ->
        match lambda
        with V(n) -> (List.mem n env)
          | P(n,m) -> (check2 m (n::env))
          | C(m1,m2) -> ((check2 m1 env) && (check2 m2 env))
    in
    (check2 lambda []) 
