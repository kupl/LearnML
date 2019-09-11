type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let ex_env = []


let rec find : var -> var list -> bool
= fun x env ->
  match env with
    | [] -> false
    | hd :: tl -> if hd = x then true else find x tl


and check : lambda -> bool
= fun lam ->
  let rec check_ : lambda -> var list -> bool
  = fun lam ev ->
    match lam with
      | P(x, l) ->  check_ l (ev @ [x])
      | V(x) -> find x ev
      | C(l1, l2) -> (check_ l1 ev) && (check_ l2 ev)
  in check_ lam ex_env;;
    
    

