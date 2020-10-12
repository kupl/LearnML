type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check: lambda -> bool = function m ->
  let rec check: lambda * (var list) -> bool = fun (m, nl) ->
    match m with
      | V(n) -> (List.exists (fun _n -> _n = n) nl)
      | P(n, _m) -> check(_m, n::nl)
      | C(m1, m2) -> check(m1, nl) && check(m2, nl)
  in check (m, [])
