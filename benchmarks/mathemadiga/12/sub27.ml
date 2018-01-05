type exp = X
		|INT of int
		|REAL of float
		|SUB of exp * exp
		|ADD of exp * exp
		|MUL of exp * exp
		|DIV of exp * exp
		|SIGMA of exp * exp * exp
		|INTEGRAL of exp * exp * exp

exception FreeVariable
exception InvalidSigma

let rec mathemadiga exp =
	let rec sigma i n ep =
		let rec mat ex =
			match (i,ex) with
			|(INT a, X) -> (float_of_int a)
			|(INT a, INT s) -> (float_of_int s)
			|(INT a, REAL s) -> s
			|(_, ADD(a,b)) -> (mat a)+.(mat b)
			|(_,SUB(a,b)) -> (mat a) -. (mat b)
			|(_,MUL(a,b)) -> (mat a)*.(mat b)
			|(_,DIV(a,b)) -> (mat i)/.(mat b)
			|(_,SIGMA(e1,e2,e3)) -> sigma (REAL (mat e1)) (REAL (mat e2)) e3
			|(_,INTEGRAL(e1,e2,e3)) -> integral (REAL (mat e1)) (REAL (mat e2)) e3
			| _ -> raise InvalidSigma
		in
		match (i,n) with
			|(INT a, INT b) ->
				if a > b then 0.0
				else (mat ep) +. (sigma (INT (a+1)) n ep)
			|(REAL a, _) -> (sigma (INT (int_of_float a)) n ep)
			|(_,REAL b) -> (sigma i (INT (int_of_float b)) ep)
			| _ -> raise InvalidSigma
	and	integral x lim ep =
		let rec mat2 ex =
			match ex with
			|X -> (mathemadiga x)
			|INT s -> (float s)
			|REAL b -> b
			|ADD(a,b)-> (mat2 a) +. (mat2 b)
			|SUB(a,b) -> (mat2 a) -. (mat2 b)
			|MUL(a,b) -> (mat2 a) *. (mat2 b)
			|DIV(a,b) -> (mat2 a) /. (mat2 b)
			|SIGMA(e1,e2,e3) -> sigma (REAL (mat2 e1)) (REAL (mat2 e2)) e3
			|INTEGRAL(e1,e2,e3) -> integral (REAL (mat2 e1)) (REAL (mat2 e2)) e3
		in
		let s1 = mathemadiga x in
		let s2 = mathemadiga lim in
		if s1 > s2 then 0.0-.(integral (REAL s2) (REAL s1) ep)
		else if s1 == s2 then 0.0
			 else if (s2-.0.1) < s1 then (s2-.s1)*.(mat2 ep)
				  else 0.1*.(mat2 ep)+.(integral (REAL (s1+.0.1)) (REAL s2) ep) 
		
	in
	match exp with
	|INT x -> (float x)
	|REAL s -> s
	|ADD(a,b) -> (mathemadiga a)+.(mathemadiga b)
	|SUB(a,b) -> (mathemadiga a)-.(mathemadiga b)
	|MUL(a,b) -> (mathemadiga a)*.(mathemadiga b)
	|DIV(a,b) -> (mathemadiga a)/.(mathemadiga b)
	|SIGMA(e1,e2,e3) -> (sigma e1 e2 e3)
	|INTEGRAL(e1,e2,e3) -> (integral e1 e2 e3)
	|_ -> raise FreeVariable
