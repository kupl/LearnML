type lambda = V of var
            |P of var * lambda
            |C of lambda * lambda
and var = string

let rec check2 var_lambda = 
  match var_lambda with
    C (m1, m2) -> (check2 m1)@(check2 m2)
    |P (n,m) ->  (List.filter (fun x -> (x  != n)) (check2 m))
    |V n -> n::[]
let check var_lambda = (List.length (check2 var_lambda)) == 0
