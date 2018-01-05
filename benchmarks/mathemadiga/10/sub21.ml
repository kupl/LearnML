exception ERROR of string
exception FreevarError
exception DividedByZero

type exp = X
	|INT of int
	|REAL of float
	|ADD of exp * exp
	|SUB of exp * exp
	|MUL of exp * exp
	|DIV of exp * exp
	|SIGMA of exp * exp * exp
	|INTEGRAL of exp * exp * exp

let rec mathemadiga t =
	
	let rec mathemadig t x i =
	match t with
	X -> if i then x else raise (FreevarError)
	|INT t -> (float_of_int t)
	|REAL t -> t
	|ADD(t1, t2) -> (mathemadig t1 x i) +. (mathemadig t2 x i)
	|SUB(t1, t2) -> (mathemadig t1 x i) -. (mathemadig t2 x i)
	|MUL(t1, t2) -> (mathemadig t1 x i) *. (mathemadig t2 x i)
	|DIV(t1, t2) -> if (mathemadig t2 x i) = 0.0 then raise (DividedByZero) else (mathemadig t1 x i) /. (mathemadig t2 x i)
	|SIGMA(t1, t2, t3) -> if (mathemadig t1 x i) < (mathemadig t2 x i) then (mathemadig t3 (mathemadig t1 x i) true) +. (mathemadig (SIGMA(REAL ((mathemadig t1 x i) +. 1.0), t2, t3)) x i)
			else if (mathemadig t1 x i) = (mathemadig t2 x i) then (mathemadig t3 (mathemadig t1 x i) true)
			else raise (ERROR "invalid input")
	|INTEGRAL(t1, t2, t3) ->  if (mathemadig t1 x i) = (mathemadig t2 x i) then 0.0
			else if (mathemadig t1 x i) > (mathemadig t2 x i) then 0.0 -. (mathemadig (INTEGRAL(t2, t1, t3)) x i)
			else if ((mathemadig t2 x i) -. (mathemadig t1 x i)) < 0.1 then ((mathemadig t2 x i) -. (mathemadig t1 x i)) *. (mathemadig t3 (mathemadig t1 x i) true)
			else (0.1 *. (mathemadig t3 (mathemadig t1 x i) true)) +.(mathemadig (INTEGRAL(REAL ((mathemadig t1 x i) +. 0.1), t2, t3)) x i)
	in
	mathemadig t 0.0 false

