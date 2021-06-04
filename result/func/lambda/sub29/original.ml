type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec checkArea (n : string list) (e : lambda) : bool =
  match e with
  | P (a, b) -> checkArea (n @ [ a ]) b
  | C (a, b) -> checkArea n a && checkArea n b
  | V a -> List.exists (fun (x : string) -> x = a) n


let rec check (e : lambda) : bool =
  match e with
  | P (a, b) -> checkArea [ a ] b
  | C (a, b) -> check a && check b
  | V n -> true
