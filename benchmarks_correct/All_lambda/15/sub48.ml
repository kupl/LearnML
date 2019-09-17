type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check (input: lambda): bool =
  let rec check (input: lambda) (rules: string list): bool =
    match input with
    | V var -> List.exists (fun x -> x = var) rules
    | P (var, m0) -> check m0 (var::rules)
    | C (m0, m1) -> (check m0 rules) && (check m1 rules)
  in
  check input []
