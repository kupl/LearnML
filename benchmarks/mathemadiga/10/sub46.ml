exception Error of string;;

type exp = X
	   | INT of int
	   | REAL of float
	   | ADD of exp * exp
	   | SUB of exp * exp
	   | MUL of exp * exp
	   | DIV of exp * exp
	   | SIGMA of exp * exp * exp
	   | INTEGRAL of exp * exp * exp;;

let rec mathemadiga_base exp env = match (exp, env) with
    (X, [])-> raise (Error "Free Variable Error")
  | (X, hd::tl) -> hd
  | (INT i, _) -> float i
  | (REAL r, _) -> r
  | (ADD(e1, e2), env) -> (mathemadiga_base e1 env) +. (mathemadiga_base e2 env)
  | (SUB(e1, e2), env) -> (mathemadiga_base e1 env) -. (mathemadiga_base e2 env)
  | (MUL(e1, e2), env) -> (mathemadiga_base e1 env) *. (mathemadiga_base e2 env)
  | (DIV(e1, e2), env) -> if (mathemadiga_base e2 env) = 0.0 then raise (Error "Divide by Zero") else (mathemadiga_base e1 env) /. (mathemadiga_base e2 env)
  | (SIGMA(e1, e2, e3), env) -> if (mathemadiga_base e1 env) = (mathemadiga_base e2 env) then (mathemadiga_base e3 ((mathemadiga_base e1 env) :: env)) 
    else (mathemadiga_base e3 ((mathemadiga_base e1 env) :: env)) +. (mathemadiga_base (SIGMA(REAL((mathemadiga_base e1 env) +. 1.0), REAL(mathemadiga_base e2 env), e3)) env)
  | (INTEGRAL(e1, e2, e3), env) -> 
      let rec integral l u e = if l = u then 0.0 
      else if (u-.l) <= 0.1 then (mathemadiga_base e (l::env)) *. (u-.l)
      else ((mathemadiga_base e (l::env)) *. 0.1) +. (integral (l +. 0.1) u e) in
	if (mathemadiga_base e1 env) > (mathemadiga_base e2 env) then -1.0 *. (integral (mathemadiga_base e2 env) (mathemadiga_base e1 env) e3)
	else (integral (mathemadiga_base e1 env) (mathemadiga_base e2 env) e3);;

let mathemadiga e = (mathemadiga_base e []);;
      
