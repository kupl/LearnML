
type lambda = V of var
         | P of var * lambda
         | C of lambda * lambda
and var = string


let rec chk : lambda -> string list -> bool
= fun e env ->
	match e with
	| V (a) -> if findInEnv a env then true else false
	| P (v, lambda) -> chk lambda (v::env)
	| C (e1, e2) -> (chk e1 env) && (chk e2 env)
	
and findInEnv : string -> string list -> bool
= fun var env ->
	match env with
	| [] -> false
	|hd::tl -> if var = hd then true else findInEnv var tl


let check : lambda -> bool
=fun e -> (* TODO *)
	chk e []
