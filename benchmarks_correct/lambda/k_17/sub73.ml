(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> (*true*) (* TODO *)
  let rec findvar l v =
    match l with
    | [] -> false
    | hd::tl ->
    if hd = v then true
    else findvar tl v in
  let rec impl l e =
    match e with
    | V v -> (findvar l v)
    | P(v, e_) -> impl (v::l) e_
    | C(e1, e2) -> (impl l e1) == true && (impl l e2) == true in
  impl [] lam
