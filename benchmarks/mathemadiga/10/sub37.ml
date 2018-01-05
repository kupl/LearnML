exception DividedByZero 
exception FreevarError 
exception Error of string

type exp = X
	 | INT of int
	 | REAL of float
	 | ADD of exp * exp
	 | SUB of exp * exp
	 | MUL of exp * exp
	 | DIV of exp * exp
	 | SIGMA of exp * exp * exp
	 | INTEGRAL of exp * exp * exp

let rec div e1 e2 =
	match e2 with
	0.0 -> raise DividedByZero
	| _ -> e1 /. e2



let rec s_math x e =
		match e with
		X -> if (x == X) then (raise FreevarError) else (s_math x x)
		|INT a -> (float_of_int) a
		|REAL a -> a
		|ADD (e1, e2) -> (s_math x e1) +. (s_math x e2)
		|SUB (e1, e2) -> (s_math x e1) -. (s_math x e2)
		|MUL (e1, e2) -> (s_math x e1) *. (s_math x e2)
		|DIV (e1, e2) -> div (s_math x e1) (s_math x e2)
		|SIGMA (e1, e2, e3) -> (cal_sigma e1 e2 e3)
		|INTEGRAL (e1, e2, e3) -> (cal_integral (s_math x e1) (s_math x e2) e3) 

and cal_sigma a b e =
	match (a,b) with
	(INT x, INT y) -> if (a = b) then (s_math a e)
				  else (s_math a e) +. (cal_sigma (INT (x+1)) b e)
	| _ -> raise (Error "^^")

and cal_integral a b e =
	if ((a < b) && ((b -. a) >= 0.1)) then (0.1 *. (s_math (REAL a) e)) +. (cal_integral (a +. 0.1) b e)
	else if ((a <= b) && ((b -. a) < 0.1)) then ((b -. a) *. (s_math (REAL a) e))
	else ((-1.0) *. (cal_integral b a e))
		   

let rec mathemadiga e =
	(s_math X e) ;;



	


