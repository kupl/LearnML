
type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string


let rec chk : exp -> string list -> bool
= fun e env ->
	match e with
	| V (a) -> if findInEnv a env then true else false
	| P (v, exp) -> chk exp (v::env)
	| C (e1, e2) -> (chk e1 env) && (chk e2 env)
	
and findInEnv : string -> string list -> bool
= fun var env ->
	match env with
	| [] -> false
	|hd::tl -> if var = hd then true else findInEnv var tl


let check : exp -> bool
=fun e -> (* TODO *)
	chk e []
