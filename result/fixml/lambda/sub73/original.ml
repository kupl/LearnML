type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec comp va exorg =
  match exorg with
  | V va1 -> false
  | P (va1, ex1) -> if va = va1 then true else comp va ex1
  | C (ex1, ex2) -> comp va ex1 || comp va ex2


let rec find ex exorg =
  match ex with
  | V va1 -> comp va1 exorg
  | P (va1, ex1) -> find ex1 exorg
  | C (ex1, ex2) -> find ex1 exorg && find ex2 exorg


let rec check : lambda -> bool =
 fun e ->
  match e with
  | V va -> false
  | P (va, ex) -> find ex e
  | C (ex1, ex2) -> check ex1 && check ex2
