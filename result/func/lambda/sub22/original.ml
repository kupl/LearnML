type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check2 (lst : string list) (var : string) (lambda : lambda) : bool =
  let mklst (lst : string list) (x : string) : string list = x :: lst in

  match lambda with
  | V n -> if List.mem n lst then true else false
  | P (n, m) -> check2 (mklst lst n) var m || check2 (mklst lst n) n m
  | C (m1, m2) -> check2 lst var m1 && check2 lst var m2


let rec check (lambda : lambda) : bool =
  match lambda with
  | V n -> false
  | P (n, m) -> check2 [ n ] n m
  | C (m1, m2) -> false
