type lambda = V of var | P of var * lambda | C of lambda * lambda

and var = string

let rec isin (str, lambdar) =
  match (str, lambdar) with
  | str, V a -> false
  | str, P (a, b) -> if a = str then true else isin (str, b)
  | str, C (a, b) -> false


let rec findvar lambda lambdaression =
  match lambda with
  | V var -> isin (var, lambdaression)
  | P (var, lambdar) -> findvar lambdar lambdaression || findvar lambdar lambda
  | C (lambdar1, lambdar2) ->
      (findvar lambdar1 lambdaression || findvar lambdar1 lambdar1)
      && (findvar lambdar2 lambdaression || findvar lambdar2 lambdar2)


let rec check : lambda -> bool =
 fun lambda ->
  match lambda with
  | V var -> false
  | P (var, lambdar) -> findvar lambdar lambda
  | C (lambdar1, lambdar2) -> check lambdar1 && check lambdar2
