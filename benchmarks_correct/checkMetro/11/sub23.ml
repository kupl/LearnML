type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check met = 
  let rec proc(m, met_list) = 
    match m with
    | V(var) -> List.mem var met_list
    | P(var, lambda) -> proc(lambda, var::met_list)
    | C(a, b) -> proc(a, met_list) && proc(b, met_list)
  in
  proc(met, [])

  
