type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string

let check lambda =
  let rec iter target vars =
    match target with
    | V station_var ->
      (List.length (List.filter (fun x -> x = station_var) vars)) > 0
    | P (area_var, new_target) -> iter new_target (area_var::vars)
    | C (lambda_a, lambda_b) -> (iter lambda_a vars)
                                    && (iter lambda_b vars)
  in
    iter lambda []
