(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec checkenv lama env =
  match lama with
  |V a -> (let rec search listl = match listl with
                                  |hd::tl -> if hd = a then true else search tl
                                  |_ -> false
           in if search env then true else false)
  |P (a,b) -> checkenv b (a::env)
  |C (a,b) -> checkenv a env && checkenv b env
  in checkenv lam []
