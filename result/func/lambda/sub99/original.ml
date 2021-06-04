type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec evalCheck1 (ex : lambda) (bd : string) : bool =
  match ex with
  | V x -> false
  | P (arg, body) -> if arg = bd then true else evalCheck1 body bd
  | C (a, b) -> evalCheck1 a bd || evalCheck1 b bd


let rec evalCheck2 (ex : lambda) (bd : lambda) : bool =
  match bd with
  | V x -> evalCheck1 ex x
  | P (arg, body) -> evalCheck2 ex body
  | C (a, b) -> evalCheck2 ex a && evalCheck2 ex b


let rec check (lambda : lambda) : bool =
  match lambda with
  | V x -> false
  | P (arg, body) -> evalCheck2 lambda body
  | C (a, b) -> check a && check b
