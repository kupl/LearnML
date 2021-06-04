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


let rec check (lambda : lambda) : bool = checkOriginal lambda lambda
