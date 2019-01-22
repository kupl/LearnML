  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
let rec seek : var -> var list -> bool
= fun v li ->
  match li with
  | [] -> false
  | hd :: tl -> if hd = v then true else seek v tl

let check : exp -> bool
= fun e ->
  let rec check_sub : exp -> var list -> bool
  = fun e li ->
    match e with
    | V v -> seek v li
    | P (v , e) -> check_sub e (v :: li)
    | C (e1, e2) -> (check_sub e1 li) && (check_sub e2 li) in
  check_sub e []

