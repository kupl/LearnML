type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let check : lambda -> bool = fun m ->
  let rec check_help : (lambda -> var list -> bool) = fun m areas ->
    match m with
    | V v -> List.mem v areas
    | P (a, b) -> check_help b (a::areas)
    | C (a, b) -> (check_help a areas) && (check_help b areas)
  in check_help m []
