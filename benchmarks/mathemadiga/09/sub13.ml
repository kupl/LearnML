(*2006-11720 2-5 KimEunSol*)
type exp = X
		|INT of int
		|REAL of float
		|ADD of exp * exp
		|SUB of exp * exp
		|MUL of exp * exp
		|DIV of exp * exp
		|SIGMA of exp * exp * exp
		|INTEGRAL of exp * exp * exp

let rec mathemadiga(a) = 
	let rec eval(x,y) = match x with
	X -> (match y with INT c -> Int32.to_float(Int32.of_int(c)) | REAL c -> c)
	|INT a -> Int32.to_float(Int32.of_int(a))
	|REAL a -> a
	|ADD(a,b) -> eval(a, y) +. eval(b, y)
	|SUB(a,b) -> eval(a, y) -. eval(b, y)
	|MUL(a,b) -> eval(a, y) *. eval(b, y)
	|DIV(a,b) -> eval(a, y) /. eval(b, y)
	in

	match a with INT b -> (Int32.to_float(Int32.of_int(b)))
	|REAL b -> b
	|ADD(b, c) -> ( mathemadiga(b) +. mathemadiga(c) )
	|SUB(b, c) -> ( mathemadiga(b) -. mathemadiga(c) )
	|MUL(b, c) -> ( mathemadiga(b) *. mathemadiga(c) )
	|DIV(b, c) -> ( mathemadiga(b) /. mathemadiga(c) )
	|SIGMA(b, c, d) -> (if mathemadiga(b) > mathemadiga(c) then 0.0
						else eval(d, b) +. mathemadiga(SIGMA(REAL(mathemadiga(b) +. 1.0), c, d)) )
	|INTEGRAL(b, c, d) -> (
			if mathemadiga(b) >= mathemadiga(c) then Int32.to_float(Int32.of_int(0))
			else eval(d, b)*.0.1 +. mathemadiga(INTEGRAL(REAL(mathemadiga(b)+.0.1), c, d)) )


