
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let check : lambda -> bool
  = fun lambda -> 
  let rec m x l = 
    match l with
    | [] -> false
    | hd :: tl -> if hd = x then true else m x tl
  in let rec find ex l = 
    match ex with
    | P(x, y) -> find y (x :: l) 
    | C(x, y) -> find x l && find y l
    | V(x) -> m x l
  in find lambda []
