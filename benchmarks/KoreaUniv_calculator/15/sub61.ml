type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

exception Error

let calculator : exp -> int
=fun e -> let rec calc e env = match (e, env) with
        | (X, []) -> raise Error
        | (X, hd::tl) -> hd
        | (INT i,_) -> i
        | (ADD (a, b), env) -> calc a env + calc b env
        | (SUB (a, b), env) -> calc a env - calc b env
        | (DIV (a, b), env) -> if calc b env == 0 then raise Error else calc a env / calc b env
        | (MUL (a, b), env) -> calc a env * calc b env
        | (SIGMA (a, b, c), env) ->
                if calc a env > calc b env then raise Error
	        else if (calc b env - calc a env) < 1 then (calc c (calc a env::env))
		else (calc c (calc a env::env)) + (calc (SIGMA (INT (calc a env + 1), INT (calc b env), c)) env)
	in calc e [] ;;
