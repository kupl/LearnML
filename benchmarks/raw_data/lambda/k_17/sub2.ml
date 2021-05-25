(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type lamEnv = (var * lambda) list
(* initailze env *)
let empty_lamEnv = []
let extend_lamEnv (x, v) e = (x,v)::e
let rec lookup_lamEnv x e = 
  match e with
  | [] -> false
  | (y,v)::tl -> if x = y then true else lookup_lamEnv x tl


let rec check_inter =
  fun lam lamEnv ->
  match lam with
  | P (v, l) -> let lamEnv' = extend_lamEnv (v, l) lamEnv in
  check_inter l lamEnv'
  | C (l1, l2) ->
    let o1 = check_inter l1 lamEnv in
    let o2 = check_inter l2 lamEnv in
    o1&&o2
  | V v -> lookup_lamEnv v lamEnv


let rec check : lambda -> bool
= fun lam -> check_inter lam empty_lamEnv
