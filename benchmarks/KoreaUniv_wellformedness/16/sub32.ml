
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec check_val : exp * (var list) -> bool
= fun (exp, lst) ->
   match exp with
   | V x -> (match lst with
             | [] -> false
             | hd::tl -> if x=hd then true else check_val(V x, tl))
   | P (x, e1) -> check_val(e1, [x]@lst)
   | C (e1, e2) -> if (check_val(e1, lst)=true && check_val(e2, lst)=true) 
                   then true else false;;

let check : exp -> bool
= fun exp -> check_val (exp, []);;
