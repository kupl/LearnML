exception FreeVariable
type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp

let rec galculator e = match e with
	X	-> raise FreeVariable
	|INT(f)	-> float_of_int f
	|REAL(f)	-> f
	|ADD(ea, eb)	-> (galculator ea) +. (galculator eb)
	|SUB(ea, eb)	-> (galculator ea) -. (galculator eb)
	|MUL(ea, eb)	-> (galculator ea) *. (galculator eb)
	|DIV(ea, eb)	-> (galculator ea) /. (galculator eb)
	|SIGMA(ea, eb, ec)	
		-> gsigma(int_of_float (galculator ea), int_of_float (galculator eb), etof_i ec, 0)
	|INTEGRAL(ea, eb, ec)	
		-> ginte(galculator ea, galculator eb, etof_f ec, 0.0)
and gsigma (ea, eb, ec, sum) =
	if ea > eb then float_of_int sum else
	gsigma (ea + 1, eb, ec, sum + (ec ea))
and ginte (ea, eb, ec, sum) = 
	if (((ea <= eb) && ((ea +. 0.1) > eb)) || ((eb <= ea) && ((eb +. 0.1) > ea))) then sum
	else if ea <= eb then
	ginte (ea +. 0.1, eb, ec, sum +. (ec ea) *. 0.1)
	else if ea >= eb then
	ginte (ea +. 0.1, eb, ec, sum -. (ec ea) *. 0.1)
	else 0.0
and etof_f e  = match e with
	X	-> (fun x -> x)
	|INT(f)	-> (fun _ -> float_of_int f)
	|REAL(f) -> (fun _ -> f)
	|ADD(f, g)	-> (fun x -> addf_f ((etof_f f) x) ((etof_f g) x))
	|SUB(f, g)	-> (fun x -> subf_f ((etof_f f) x) ((etof_f g) x))
	|MUL(f, g)	-> (fun x -> mulf_f ((etof_f f) x) ((etof_f g) x))
	|DIV(f, g)	-> (fun x -> divf_f ((etof_f f) x) ((etof_f g) x))
	|e	-> (fun _ -> 0.0)
and addf_f a b = a +. b
and subf_f a b = a -. b
and mulf_f a b = a *. b
and divf_f  a b = a /. b
and etof_i e  = match e with
        X   -> (fun x -> x)
        |INT(f) -> (fun _ -> f)
        |REAL(f) -> (fun _ -> int_of_float f)
        |ADD(f, g)  -> (fun x -> addf_i ((etof_i f) x) ((etof_i g) x))
	    |SUB(f, g)  -> (fun x -> subf_i ((etof_i f) x) ((etof_i g) x))
	    |MUL(f, g)  -> (fun x -> mulf_i ((etof_i f) x) ((etof_i g) x))
	    |DIV(f, g)  -> (fun x -> divf_i ((etof_i f) x) ((etof_i g) x))
	    |e  -> (fun _ -> 0)
and addf_i a b = a + b
and subf_i a b = a - b
and mulf_i a b = a * b
and divf_i a b = a / b
