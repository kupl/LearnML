type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (e : lambda) : bool =
  match e with
  | V x -> true
  | P (x, y) -> check y && checkx (x, y)
  | C (x, y) -> check y && check x


and checkx ((a : string), (b : lambda)) : bool =
  match b with
  | V x -> if x = a then true else false
  | P (x, y) -> checkx (a, y)
  | C (x, y) -> checkx (a, x) || checkx (a, y)
