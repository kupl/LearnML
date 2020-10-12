type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec lookup var env = match env with
  | [] -> false
  | hd::tl -> if hd=var then true else lookup var tl

let rec check : lambda -> bool
= fun lam ->
  let rec alpaca env lam = match lam with (*[] lam -> bool *)
  | V x -> lookup x env 
  | P (x,lam1) -> let newenv = x::env in alpaca newenv lam1
  | C (lam1,lam2) -> (alpaca env lam1) && (alpaca env lam2)
  in alpaca [] lam;;




