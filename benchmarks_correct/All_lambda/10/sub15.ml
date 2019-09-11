type lambda = V of var
            |P of var * lambda
            |C of lambda * lambda
and var = string

let rec check var_lambda = 
  match var_lambda with
    C (m1, m2) -> (check m1)@(check m2)
    |P (n,m) ->  (List.filter (fun x -> (x  != n)) (check m))
    |V n -> n::[]
let check var_lambda = (List.length (check var_lambda)) == 0
