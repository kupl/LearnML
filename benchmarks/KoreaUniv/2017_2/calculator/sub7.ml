(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calc : exp-> (int list) -> int
= fun exp env -> match exp with
			| X -> (List.hd env)
			| INT(x) -> x
			| ADD(x, y) -> (calc x env) + (calc y env)
			| SUB(x, y) -> (calc x env) - (calc y env)
			| MUL(x, y) -> (calc x env) * (calc y env)
			| DIV(x, y) -> (calc x env) / (calc y env)
			| SIGMA(x, y, z) -> let x = (calc x env) in
					    let y = (calc y env) in
					    let rec sigm : int->int->exp->(int list)->int
					    = fun x y z env -> if x<=y then (calc z (x::env)) + (sigm (x+1) y z env) else 0 in (sigm x y z env)

let calculator : exp -> int
= fun e -> (calc e [])