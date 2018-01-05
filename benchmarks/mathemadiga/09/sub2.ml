exception ERROR

type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp

let rec  mathemadiga (ep) = 

let rec zorra (x, y) = 
		match x with
		X -> (match y with INT c -> (float_of_int(c))
	 	|REAL c -> c)	
		|INT a -> (float_of_int(a))
		|REAL a -> a
		|ADD(a, b) -> zorra(a, y) +. zorra(b, y)
		|SUB(a, b) -> zorra(a, y) -. zorra(b, y)
		|MUL(a, b) -> zorra(a, y) *. zorra(b, y)
		|DIV(a, b) -> zorra(a, y) /. zorra(b, y)
		
in

		match ep with INT b -> (float_of_int(b))
		|REAL b -> b
		|ADD(b, c) -> (mathemadiga(b) +. mathemadiga(c))
		|SUB(b, c) -> (mathemadiga(b) -. mathemadiga(c))
		|MUL(b, c) -> (mathemadiga(b) *. mathemadiga(c))
		|DIV(b, c) ->  if (mathemadiga(c)=0.0) then raise ERROR
						else (mathemadiga(b) /. mathemadiga(c))
		|SIGMA(b, c, d) ->  (if(mathemadiga(b)) > (mathemadiga(c)) then (float_of_int(0))
						 		else zorra(d, b) +. mathemadiga(SIGMA(REAL(mathemadiga(b)+.1.0), c, d)))
		|INTEGRAL(b, c, d) -> (if mathemadiga(b) >= mathemadiga(c) then (float_of_int(0))
								else zorra(d, b)*.0.1 +. mathemadiga(INTEGRAL(REAL(mathemadiga(b)+.0.1),c, d)))
