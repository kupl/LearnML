
type lambda = V of var
         | P of var * lambda
         | C of lambda * lambda
and var = string

let empty_env = []
let extend_env x e = x::e
let rec lookup_env x e = match e with
| [] -> false
| hd::tl -> if x=hd then true else lookup_env x tl

let rec check f = let rec _check fu en =
  match fu with
  | P (x,y) -> let env' = extend_env x en in _check y env'
  | V x -> if lookup_env x en then true else false
  | C (x,y) -> if _check x en = true then _check y en else false
in _check f empty_env;;
