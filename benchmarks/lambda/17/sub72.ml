type var = string
type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda

let check : lambda -> bool = fun m ->
  let rec findvar : lambda -> var list -> bool = fun n l ->
    match n with
    | V o -> List.mem o l
    | P (o, p) -> findvar p (o::l)
    | C (p, q) -> (findvar p l) && (findvar q l)
  in
  findvar m []
