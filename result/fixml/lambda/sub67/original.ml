type lambda = V of var | P of var * lambda | C of lambda * lambda

and var = string

let rec equi x l = match l with [] -> true | hd :: tl -> x = hd && equi x tl

let rec check_r : lambda -> string list -> bool =
 fun e env ->
  match e with
  | V var -> if env = [] then false else equi var env
  | P (v, e) -> check_r e (v :: env)
  | C (e1, e2) -> check_r e1 env && check_r e2 env


let check : lambda -> bool = fun e -> check_r e []
