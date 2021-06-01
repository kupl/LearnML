
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

let rec check_val : lambda * (var list) -> bool
= fun (lambda, lst) ->
   match lambda with
   | V x -> (match lst with
             | [] -> false
             | hd::tl -> if x=hd then true else check_val(V x, tl))
   | P (x, e1) -> check_val(e1, [x]@lst)
   | C (e1, e2) -> if (check_val(e1, lst)=true && check_val(e2, lst)=true) 
                   then true else false;;

let check : lambda -> bool
= fun lambda -> check_val (lambda, []);;
