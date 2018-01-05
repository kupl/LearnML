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

let rec cal (exp, float,bool) =
	match exp with
	| X -> 
		begin
			match bool with
			| true -> float
			| false -> raise (FreeVariable)
		end
  | INT (int) -> 
			 float_of_int(int)
	| REAL (float) ->
			float
	| ADD (exp1, exp2) ->
			begin
			match (exp1, exp2) with
        | (INT (a), INT (b)) -> 
    			float_of_int(a+b)
    		| (REAL (a), INT (b)) ->
    			a +. float_of_int(b)
    		| (INT (a), REAL (b)) ->
    			float_of_int(a)+.b
    		| (REAL (a), REAL (b)) ->
    			a+.b
				| (_,_) ->
					cal(exp1,float,bool) +. cal(exp2,float,bool)
			end
		| SUB (exp1, exp2) ->
			begin
			match (exp1, exp2) with
        | (INT (a), INT (b)) -> 
    			float_of_int(a-b)
    		| (REAL (a), INT (b)) ->
    			a -. float_of_int(b)
    		| (INT (a), REAL (b)) ->
    			float_of_int(a)-.b
    		| (REAL (a), REAL (b)) ->
    			a-.b
				| (_,_) ->
					cal(exp1,float,bool) -. cal(exp2,float,bool)
			end
		| MUL (exp1, exp2) ->
			begin
			match (exp1, exp2) with
        | (INT (a), INT (b)) -> 
    			float_of_int(a*b)
    		| (REAL (a), INT (b)) ->
    			a *. float_of_int(b)
    		| (INT (a), REAL (b)) ->
    			float_of_int(a)*.b
    		| (REAL (a), REAL (b)) ->
    			a*.b
				| (_,_) ->
					cal(exp1,float,bool) *. cal(exp2,float,bool)
			end
		| DIV (exp1, exp2) ->
			begin
			match (exp1, exp2) with
        | (INT (a), INT (b)) -> 
    			float_of_int(a/b)
    		| (REAL (a), INT (b)) ->
    			a /. float_of_int(b)
    		| (INT (a), REAL (b)) ->
    			float_of_int(a)/.b
    		| (REAL (a), REAL (b)) ->
    			a/.b
				| (_,_) ->
					cal(exp1,float,bool) /. cal(exp2,float,bool)
			end
		| SIGMA (exp1, exp2, exp3) ->
			if (cal(exp1,float,bool) > cal(exp2,float,bool)) then 0.
	   	else 
				let rec sigma (num1,num2,exp3, result) =	
					if (num1 > num2) then result
					else
						sigma(num1+.1.,num2, exp3, result +.cal(exp3,cal(REAL num1,0.,false),true))	in
						sigma(cal(exp1,float,bool),cal(exp2,float,bool),exp3,0.)				
	| INTEGRAL(exp1, exp2, exp3) ->
			if (cal(exp2,float,bool) > cal(exp1,float,bool)) then 
				if (cal(exp2,float,bool) -. cal(exp1,float,bool)<0.1) then 0.
				else 
					let rec integral (num1,num2,exp3, result) =	
					if ((num2-.num1) < 0.1) then result
					else
						integral(num1+.0.1,num2, exp3, result +.cal(exp3,cal(REAL num1,0.,false),true)*.0.1)	in
					integral(cal(exp1,float,bool),cal(exp2,float,bool),exp3,0.)
			else 
				if (cal(exp1,float,bool) -. cal(exp2,float,bool)<0.1) then 0.
				else
					let rec integral (num1,num2,exp3, result) =	
					if ((num2-.num1) < 0.1) then result
					else
						integral(num1+.0.1,num2, exp3, result +.cal(exp3,cal(REAL num1,0.,false),true)*.0.1)	in
					-1.*.integral(cal(exp2,float,bool),cal(exp1,float,bool),exp3,0.)
					
let galculator(exp) = cal(exp, 0., false)
	
	