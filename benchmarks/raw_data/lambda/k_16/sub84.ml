
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string;;

  let rec check : lambda -> bool
  = fun lambda -> let rec checking : lambda * (var list) -> bool
= fun (alambda, vlist) -> (match alambda with
  V s -> (match vlist with
				  [] -> false
				| hd::tl -> if s = hd then true else checking (V s, tl))
| P (v,lambda1) -> checking (lambda1, v :: vlist)
| C (lambda1, lambda2) -> (checking (lambda1, vlist))&&(checking (lambda2, vlist)))						 
in checking (lambda,[]);;
