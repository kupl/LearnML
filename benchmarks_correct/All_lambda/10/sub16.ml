exception Error of string
type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
                         and var = string

let check : lambda -> bool =
  fun lambda ->
    let rec check : lambda -> var list -> bool =
      fun lambda -> fun env ->
        match lambda
        with V(n) -> (List.mem n env)
          | P(n,m) -> (check m (n::env))
          | C(m1,m2) -> ((check m1 env) && (check m2 env))
    in
      (check lambda [])
