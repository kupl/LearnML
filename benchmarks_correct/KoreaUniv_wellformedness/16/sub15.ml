
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec check3 v l= match l with
    [] -> false
    |hd::tl -> if hd=v then true else check3 v tl

  let rec check2 e l = match e with
     V(v)->check3 v l
    |P(v, lambda1)->check2 lambda1 (l@[v])
    |C(lambda2, lambda3)->check2 lambda2 l&&check2 lambda3 l

  let check : lambda -> bool
  = fun lambda -> check2 lambda []
