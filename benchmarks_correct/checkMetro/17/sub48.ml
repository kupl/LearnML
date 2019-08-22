type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
         and var = string

let check: lambda -> bool = fun m ->
  let rec check: lambda -> string list -> bool = fun m areas ->
    match m with
    | V a -> (List.mem a areas)
    | P (a, b) -> (check b (areas @ [a]))
    | C (a, b) -> (check a areas) && (check b areas) in
  check m []

