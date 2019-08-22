type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec containCheck (n, ll) : bool =
  match ll with
  |hd::tl ->
    if hd=n then true else containCheck(n,tl)
  |[] -> false

let check lambda : bool =
  let rec check (lambda,ll) : bool =
    match lambda with
    |V n -> containCheck(n,ll)
    |P (n,met) -> check (met,n::ll)
    |C (met1,met2) -> check (met1,ll) && check(met2,ll)
  in check(lambda,[])
