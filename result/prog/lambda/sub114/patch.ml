type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec isin ((str : string), (lambdar : lambda)) : bool =
  match (str, lambdar) with
  | str, V a -> false
  | str, P (a, b) -> if a = str then true else isin (str, b)
  | str, C (a, b) -> false


let rec findvar (lambda : lambda) (lambdaression : lambda) : bool =
  match lambda with
  | V var -> isin (var, lambdaression)
  | P (var, lambdar) -> findvar lambdar lambdaression || findvar lambdar lambda
  | C (lambdar1, lambdar2) ->
      (findvar lambdar1 lambdaression || findvar lambdar1 lambdar1)
      && (findvar lambdar2 lambdaression || findvar lambdar2 lambdar2)


let rec __s1 (__s2 : lambda) (__s3 : string list) : bool =
  match __s2 with
  | V __s11 -> List.mem __s11 __s3
  | C (__s12, __s13) -> __s1 __s12 __s3 && __s1 __s13 __s3
  | P (__s14, __s15) -> __s1 __s15 (__s14 :: __s3)


let rec check (lambda : lambda) : bool =
  match lambda with
  | V var -> false
  | P (var, lambdar) -> __s1 lambdar [ var ]
  | C (lambdar1, lambdar2) -> check lambdar1 && check lambdar2
