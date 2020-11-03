type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let check lambda =
  let rec check_helper m var_list = 
    match m with
    |V var -> List.mem var var_list
    |P (var, m1) -> check_helper m1 (var::var_list)
    |C (m1, m2) -> (check_helper m1 var_list) && (check_helper m2 var_list)
  in
  check_helper lambda []

