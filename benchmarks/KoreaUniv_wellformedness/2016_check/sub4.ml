  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let check : exp -> bool
  = fun exp -> 
    let rec removeMatch : (var list) -> var -> (var list)
    = fun l v ->
      match l with
      | [] -> []
      | hd::tl -> 
        if hd = v then removeMatch tl v
        else hd::(removeMatch tl v) in
    let rec eval : exp -> (var list)
    = fun exp ->
      match exp with
      | V v -> [v]
      | P(v,e) -> removeMatch (eval e) v
      | C(e1,e2) -> (eval e1)@(eval e2)
    in if (eval exp) = [] then true
       else false 
