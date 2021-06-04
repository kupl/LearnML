type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec has (str : string) (lambdaression : lambda) : bool =
  match lambdaression with
  | V v -> false
  | P (v, e) -> if v = str then true else false
  | C (e1, e2) -> has str e1 || has str e2


let rec checkOriginal (lambda : lambda) (original : lambda) : bool =
  match lambda with
  | V v -> has v original
  | P (v, e) -> checkOriginal e original
  | C (e1, e2) -> checkOriginal e1 original && checkOriginal e2 original


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (lambda : lambda) : bool = List.length (__s3 lambda) = 0
