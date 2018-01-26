
(* problem 5*)
type exp = X
        | INT of int
        | ADD of exp * exp
        | SUB of exp * exp
        | MUL of exp * exp
        | DIV of exp * exp
        | SIGMA of exp * exp * exp

type env = exp * int list
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env x e =
  match e with
  | [] -> 0
  | (y,v)::tl -> if x = y then v else apply_env x tl
        
let rec calculator : exp -> int
= fun e -> let rec help_cal e env 
            = match e with
             | X -> apply_env X env
             | INT(x) -> x
             | ADD(x,y) -> (help_cal x env) + (help_cal y env)
             | SUB(x,y) -> (help_cal x env) - (help_cal y env)
             | MUL(x,y) -> (help_cal x env) * (help_cal y env)
             | DIV(x,y) -> (help_cal x env) / (help_cal y env)
             | SIGMA(x,y,z) -> match (help_cal x env), (help_cal y env) with 
                                 | n1, n2 -> 
                                    if n1 = n2 then (let v1 = extend_env (X,n1) env
                                                     in help_cal z v1)
                                    else ((let v2 = extend_env (X,n1) env
                                          in help_cal z v2) + (help_cal (SIGMA(INT(n1+1),y,z)) env))
            in help_cal e empty_env
