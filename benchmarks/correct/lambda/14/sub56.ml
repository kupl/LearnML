type lambda =
    | V of var
    | P of var * lambda
    | C of lambda * lambda
and var = string;;

let rec check lambda =
    let rec checkWithNames lambda vars =
        match lambda with
        | V var -> List.exists (fun x -> x = var) vars
        | P (var, lambda) -> checkWithNames lambda (var::vars)
        | C (lambda0, lambda1) -> (checkWithNames lambda0 vars) &&
        (checkWithNames lambda1 vars)
    in
    checkWithNames lambda [];;
