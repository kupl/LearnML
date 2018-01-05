type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

exception UNDEFINED_X

let mathemadiga expr = 
	let rec eval : float list * exp -> float = fun(l,e) ->
	match e with
	|X -> (match l with
		|[] -> raise UNDEFINED_X
		|h::_ -> h)
	|INT(i)  -> float_of_int i (* converts to float *)
	|REAL(r) -> r
	|ADD(e1,e2) -> eval(l,e1) +. eval(l,e2)
	|SUB(e1,e2) -> eval(l,e1) -. eval(l,e2)
	|MUL(e1,e2) -> eval(l,e1) *. eval(l,e2)
	|DIV(e1,e2) -> eval(l,e1) /. eval(l,e2)
	|SIGMA(s,e,f) -> let ns = int_of_float (eval(l,s)) in
                     let ne = int_of_float (eval(l,e)) in 
                     if ns > ne then 0.0
			         else eval([float_of_int ns],f) +. eval(l,SIGMA(INT (ns+1),e,f))
	|INTEGRAL(s,e,f) -> let dt = 0.1 in
			            let sv = eval(l,s) in
			            let ev = eval(l,e) in
			            if sv > ev then -1.0 *. eval(l,INTEGRAL(e,s,f))
			            else if sv > ev -. dt then 0.0 (* smaller then dt *)
			            else eval([sv],f) *. dt +. eval(l,INTEGRAL(REAL (sv+.dt),e,f))
	in eval([],expr)

