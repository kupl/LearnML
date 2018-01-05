(* 컴퓨터공학부/2006-11855/정용혁/HW2-ex5 *)

type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp * exp
		 | INTEGRAL of exp * exp * exp
		 
exception FreeVariable
exception InvalidSigma
exception DivideByZero

let rec mathemadiga expr =
  let rec eval expr env =
    match expr with
	  X -> if (List.length env = 0) then raise FreeVariable
	       else (List.hd env)
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> (eval e1 env) +. (eval e2 env)
	| SUB (e1, e2) -> (eval e1 env) -. (eval e2 env)
	| MUL (e1, e2) -> (eval e1 env) *. (eval e2 env)
	| DIV (e1, e2) ->  let v2 = (eval e2 env) in
	    if v2 = 0.0 then raise DivideByZero else (eval e1 env) /. v2
	| SIGMA (low, high, e) ->
		let l = (eval low env) in
		let h = (eval high env) in
	    if (l > h) then raise InvalidSigma
		else if (h -. l < 1.0) then (eval e [l])
	    else (eval e [l]) +. (eval (SIGMA(REAL(l +. 1.0), high, e)) env)
	| INTEGRAL (low, high, e) ->
	    let l = (eval low env) in
		let h = (eval high env) in
		if (l > h) then -. (eval (INTEGRAL(high, low, e)) env)
	    else if (h -. l < 0.1) then (eval e [l]) *. (h -. l)
		else (eval e [l]) *. 0.1 +. (eval (INTEGRAL(REAL(l +. 0.1), high, e)) env)
  in
    (eval expr [])
