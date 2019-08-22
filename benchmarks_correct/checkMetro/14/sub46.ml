(* hw 2-1 *)
(* 2012-11269 DongJae Lim *)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec cM ((m : lambda), (ml : var list)) : bool =
  match m with
  | V (n0) -> (List.mem n0 ml)
  | P (n0, m0) -> (cM (m0, ml @ [n0]))
  | C (m0, m1) -> (cM (m0, ml)) && (cM (m1, ml))

let check (m : lambda) : bool =
  cM (m, [])
