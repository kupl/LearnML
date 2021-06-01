
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

let rec compare v l = match l with
[]-> false
|hd::tl -> if hd=v then true else compare v tl

let rec makel e l = match e with
V(v) -> compare v l
|P(v,lambda1)-> makel lambda1 (l@[v])
|C(lambda2,lambda3) -> makel lambda2 l&&makel lambda3 l;;
  let check : lambda -> bool
  = fun lambda -> makel lambda [];;
