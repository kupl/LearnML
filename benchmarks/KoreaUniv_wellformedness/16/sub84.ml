
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string;;

  let rec check : exp -> bool
  = fun exp -> let rec checking : exp * (var list) -> bool
= fun (aexp, vlist) -> (match aexp with
  V s -> (match vlist with
				  [] -> false
				| hd::tl -> if s = hd then true else checking (V s, tl))
| P (v,exp1) -> checking (exp1, v :: vlist)
| C (exp1, exp2) -> (checking (exp1, vlist))&&(checking (exp2, vlist)))						 
in checking (exp,[]);;
