type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string
  
  let rec check2: lambda * var list -> bool = function 
    (current_lambda, area_list) -> 
      match current_lambda with
        | P(area_var, sub_lambda) -> check2(sub_lambda, area_var::area_list)
        | C(sub_lambda_1, sub_lambda_2) -> check2(sub_lambda_1, area_list) && check2(sub_lambda_2, area_list)
        | V(station_var) -> List.mem station_var area_list
          
          
  let check: lambda -> bool = function(_lambda) -> check2(_lambda, [])
    