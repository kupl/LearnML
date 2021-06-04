type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec isthere (key : string) (lambda : lambda) : lambda =
  match lambda with
  | V v1 -> if key = v1 then V "true" else V v1
  | P (v1, e1) -> P (v1, isthere key (isthere v1 e1))
  | C (e1, e2) -> C (isthere key e1, isthere key e2)


let rec nothere (lambda : lambda) : bool =
  match lambda with
  | V v -> if v = "true" then true else false
  | P (v, e) -> if nothere e = true then true else false
  | C (e1, e2) -> if nothere e1 = true && nothere e2 = true then true else false


let rec check (lambda : lambda) : bool =
  match lambda with
  | V v -> false
  | P (v, e) -> nothere (isthere v e)
  | C (e1, e2) -> false
