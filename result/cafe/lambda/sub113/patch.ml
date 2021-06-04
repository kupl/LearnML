type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec exist v (lst : 'a list) : bool =
  match lst with [] -> false | hd :: tl -> if v = hd then true else exist v tl


let rec checker (e : lambda) (lst : string list) : bool =
  match e with
  | V v -> exist v lst
  | P (v, e) -> checker e (lst @ [ v ])
  | C (e1, e2) -> checker e1 lst && checker e2 lst


let rec check (lambda : lambda) : bool = checker lambda []
