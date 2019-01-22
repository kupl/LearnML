  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let append_item lst a = lst @ [a]
 
  let rec equiv lst a = match lst with
  | [] -> false
  | last::[] -> if a=last then true else false
  | hd::tl -> if a=hd then true else equiv tl a

  let rec check_scope
  =fun e lst -> match e with
  | V(a) -> equiv lst a 
  | P(a,e) -> check_scope e (append_item lst a)
  | C(e1,e2) -> (check_scope e1 lst) && (check_scope e2 lst)

  let check : exp -> bool
  =fun e -> let init = [] in check_scope e init
