type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let check : lambda -> bool
= fun lam -> 
  let rec find
  = fun v lst ->
    match lst with
    |[] -> false
    |hd::tl -> if hd = v then true else find v tl in
  let rec ch
  = fun la lst ->
    match la with
    |V v -> find v lst
    |P (v, l) -> ch l (v::lst)
    |C (l1, l2) -> ch l1 lst && ch l2 lst in
  ch lam [];;