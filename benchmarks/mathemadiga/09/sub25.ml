exception FreeVariable
exception InvalidSigma
exception DivideByZero

type exp = X
  		| INT of int
  		| REAL of float
  		| ADD of exp * exp
  		| SUB of exp * exp
  		| MUL of exp * exp
  		| DIV of exp * exp
  		| SIGMA of exp * exp * exp
  		| INTEGRAL of exp * exp * exp;;

let rec mathemadiga expin:float =
	let err = ref true in
	let rec toReal expinrec par =
		match expinrec with
			X -> par
			| INT(x) -> (float_of_int x)
			| REAL(x) -> x
			| ADD(x,y) -> (toReal x par) +. (toReal y par)
			| SUB(x,y) -> (toReal x par) -. (toReal y par)
			| MUL(x,y) -> (toReal x par) *. (toReal y par)
			| DIV(x,y) -> if (toReal y par) = 0.0 then raise DivideByZero else (toReal x par) /. (toReal y par)
			| SIGMA(a,n,f) -> if !err then (if (toReal a 0.0) > (toReal n 0.0) then raise InvalidSigma else ((err:=false);(toReal expinrec 0.0)) ) else if (toReal a 0.0) > (toReal n 0.0) then 0.0 else (toReal f (toReal a 0.0) ) +. (toReal (SIGMA( ADD( a , INT(1) ) , n ,f)) (toReal a 0.0) )
			| INTEGRAL(a,b,f) -> if !err then (if (toReal a 0.0) > (toReal b 0.0) then (err:=false;(toReal (SUB( INT 0,(INTEGRAL(b,a,f)))) 0.0) ) else (err:=false;(toReal (INTEGRAL(a,b,f)) 0.0 ))) else (if (toReal a 0.0) < (toReal b 0.0) then (if (toReal b 0.0) -. (toReal a 0.0) > 0.1 then (toReal (MUL((REAL 0.1),f)) ( toReal a 0.0)) +. (toReal (INTEGRAL(ADD(a,REAL 0.1),b,f)) 0.0) else (toReal (MUL(SUB(b,a),f)) (toReal a 0.0))) else 0.0)
	in
	toReal expin 0.0;;
