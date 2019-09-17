  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let check : lambda -> bool
  = fun lambda -> 
    let rec removeMatch : (var list) -> var -> (var list)
    = fun l v ->
      match l with
      | [] -> []
      | hd::tl -> 
        if hd = v then removeMatch tl v
        else hd::(removeMatch tl v) in
    let rec eval : lambda -> (var list)
    = fun lambda ->
      match lambda with
      | V v -> [v]
      | P(v,e) -> removeMatch (eval e) v
      | C(e1,e2) -> (eval e1)@(eval e2)
    in if (eval lambda) = [] then true
       else false 
