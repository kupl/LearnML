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


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (lambda : lambda) : bool = List.length (__s3 lambda) = 0
