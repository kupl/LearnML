
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let check : exp -> bool
  = fun exp -> 
  let rec m x l = 
    match l with
    | [] -> false
    | hd :: tl -> if hd = x then true else m x tl
  in let rec find ex l = 
    match ex with
    | P(x, y) -> find y (x :: l) 
    | C(x, y) -> find x l && find y l
    | V(x) -> m x l
  in find exp []
