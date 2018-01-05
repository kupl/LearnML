type exp = X | INT of int | REAL of float | ADD of exp*exp | SUB of exp*exp | MUL of exp*exp | DIV of exp*exp | SIGMA of exp*exp*exp | INTEGRAL of exp*exp*exp 

exception Error of exp

let rec mathemadiga (exp00) =
        match exp00 with
	| X -> raise (Error exp00)
	| INT(int00) -> (float int00)
        | REAL(float00) -> float00
        | ADD(exp1, exp2) -> (mathemadiga(exp1) +. mathemadiga(exp2))
        | SUB(exp1, exp2) -> (mathemadiga(exp1) -. mathemadiga(exp2))
        | MUL(exp1, exp2) -> (mathemadiga(exp1) *. mathemadiga(exp2))
        | DIV(exp1, exp2) -> (mathemadiga(exp1) /. mathemadiga(exp2))
	| SIGMA(exp1, exp2, exp3) ->
		begin
		match exp1 with
		| INT(int01) ->
			begin
			match exp2 with
			| INT(int02) ->
				begin 
				if (int01 = int02) then
					begin
					match exp3 with
					| X -> (mathemadiga(exp1))
					| INT(int00) -> (float int00)
					| REAL(float00) -> float00
					| ADD(exp4,exp5) ->
						begin
						if (exp4 = X && exp5 = X) then ((mathemadiga(exp1)) *. 2.0)					
						else if (exp4 = X) then ((mathemadiga (exp1)) +. (mathemadiga (SIGMA (exp1, exp1, exp5))))			
						else if (exp5 = X) then ((mathemadiga (SIGMA (exp1, exp1, exp4))) +. (mathemadiga (exp1)))
						else ((mathemadiga (SIGMA (exp1, exp1, exp4))) +. (mathemadiga (SIGMA (exp1, exp1, exp5)))) 
						end	
					| SUB(exp4,exp5) ->
						begin
						if (exp4 = X && exp5 = X) then 0.0					
						else if (exp4 = X) then ((mathemadiga (exp1)) -. (mathemadiga (SIGMA (exp1, exp1, exp5))))			
						else if (exp5 = X) then ((mathemadiga (SIGMA (exp1, exp1, exp4))) -. (mathemadiga (exp1)))
						else ((mathemadiga (SIGMA (exp1, exp1, exp4))) -. (mathemadiga (SIGMA (exp1, exp1, exp5)))) 
						end	
					| MUL(exp4,exp5) ->
						begin
						if (exp4 = X && exp5 = X) then ((mathemadiga(exp1)) *. (mathemadiga(exp1)))					
						else if (exp4 = X) then ((mathemadiga (exp1)) *. (mathemadiga (SIGMA (exp1, exp1, exp5))))			
						else if (exp5 = X) then ((mathemadiga (SIGMA (exp1, exp1, exp4))) *. (mathemadiga (exp1)))
						else ((mathemadiga (SIGMA (exp1, exp1, exp4))) *. (mathemadiga (SIGMA (exp1, exp1, exp5)))) 
						end	
					| DIV(exp4,exp5) ->
						begin
						if (exp4 = X && exp5 = X) then 1.0					
						else if (exp4 = X) then ((mathemadiga (exp1)) /. (mathemadiga (SIGMA (exp1, exp1, exp5))))			
						else if (exp5 = X) then ((mathemadiga (SIGMA (exp1, exp1, exp4))) /. (mathemadiga (exp1)))
						else ((mathemadiga (SIGMA (exp1, exp1, exp4))) /. (mathemadiga (SIGMA (exp1, exp1, exp5)))) 
						end
					| SIGMA(exp4,exp5,exp6) -> (mathemadiga exp3)
					| INTEGRAL(exp4,exp5,exp6) -> (mathemadiga exp3)
					end
				else if ((mathemadiga exp1) < (mathemadiga exp2))
					then ((mathemadiga (SIGMA (exp1, exp1, exp3))) +. (mathemadiga (SIGMA ((INT (int_of_float ((mathemadiga(exp1)) +. 1.0 ))), exp2, exp3))))
				else raise (Error exp00) 
				end
			| _ -> raise (Error exp00)
			end
		| _ -> raise (Error exp00)
		end
	| INTEGRAL(exp1, exp2, exp3) -> 
		begin                                                                         
		match exp1 with
		| INT(int01) ->
			begin
			match exp2 with
			| REAL(float02) -> (mathemadiga (INTEGRAL ((REAL (mathemadiga (exp1))), exp2, exp3)))
			| INT(int02) -> (mathemadiga (INTEGRAL ((REAL (mathemadiga (exp1))), (REAL (mathemadiga (exp2))), exp3)))
			| _ -> raise (Error exp00)
			end
		| REAL(float01) ->
			begin
			match exp2 with
			| REAL(float02) ->
 				begin 
				if (float01 = float02) then 0.0
				else if (mathemadiga (exp2) -. mathemadiga(exp1)) <= 0.1 then
					begin
					match exp3 with
					| X -> (mathemadiga exp1) *. (float02 -. float01)
					| INT(int00) -> ((float int00) *. (float02 -. float01))
					| REAL(float00) -> (float00 *. (float02 -. float01))
					| ADD (exp4, exp5) ->
						begin
						if (exp4 = X && exp5 = X) then (((mathemadiga(exp1)) *. 2.0) *. (float02 -. float01))					
						else if (exp4 = X) then (((mathemadiga(exp1)) *. (float02 -. float01)) +. ((mathemadiga (INTEGRAL (exp1, exp2, exp5))) *. (float02 -. float01)))			
						else if (exp5 = X) then (((mathemadiga (INTEGRAL (exp1, exp2, exp4))) *. (float02 -. float01)) +. ((mathemadiga(exp1)) *. (float02 -. float01)))
						else (((mathemadiga (INTEGRAL (exp1, exp2, exp4))) *. (float02 -. float01)) +. ((mathemadiga (INTEGRAL (exp1, exp2, exp5))) *. (float02 -. float01))) 
						end					
					| SUB(exp4,exp5) ->
						begin
						if (exp4 = X && exp5 = X) then 0.0
						else if (exp4 = X) then (((mathemadiga(exp1)) *. (float02 -. float01)) -. ((mathemadiga (INTEGRAL (exp1, exp2, exp5))) *. (float02 -. float01)))			
						else if (exp5 = X) then (((mathemadiga (INTEGRAL (exp1, exp2, exp4))) *. (float02 -. float01)) -. ((mathemadiga(exp1)) *. (float02 -. float01)))
						else (((mathemadiga (INTEGRAL (exp1, exp2, exp4))) *. (float02 -. float01)) -. ((mathemadiga (INTEGRAL (exp1, exp2, exp5))) *. (float02 -. float01))) 
						end					
					| MUL(exp4,exp5) ->
						begin
						if (exp4 = X && exp5 = X) then (((mathemadiga(exp1)) *. (float02 -. float01)) *. ((mathemadiga(exp1)) *. (float02 -. float01)))					
						else if (exp4 = X) then (((mathemadiga(exp1)) *. (float02 -. float01)) *. ((mathemadiga (INTEGRAL (exp1, exp2, exp5))) *. (float02 -. float01)))			
						else if (exp5 = X) then (((mathemadiga (INTEGRAL (exp1, exp2, exp4))) *. (float02 -. float01)) *. ((mathemadiga(exp1)) *. (float02 -. float01)))
						else (((mathemadiga (INTEGRAL (exp1, exp2, exp4))) *. (float02 -. float01)) *. ((mathemadiga (INTEGRAL (exp1, exp2, exp5))) *. (float02 -. float01))) 
						end					
					| DIV(exp4,exp5) ->
						begin
						if (exp4 = X && exp5 = X) then ((float02 -. float01) *. 1.0)					
						else if (exp4 = X) then (((mathemadiga(exp1)) *. (float02 -. float01)) /. ((mathemadiga (INTEGRAL (exp1, exp2, exp5))) *. (float02 -. float01)))			
						else if (exp5 = X) then (((mathemadiga (INTEGRAL (exp1, exp2, exp4))) *. (float02 -. float01)) /. ((mathemadiga(exp1)) *. (float02 -. float01)))
						else (((mathemadiga (INTEGRAL (exp1, exp2, exp4))) *. (float02 -. float01)) /. ((mathemadiga (INTEGRAL (exp1, exp2, exp5))) *. (float02 -. float01))) 
						end					
					| SIGMA(exp4,exp5,exp6) -> (mathemadiga exp3)
					| INTEGRAL(exp4, exp5, exp6) -> (mathemadiga exp3)
					end
				else if (float02 -. float01) > 0.1 then 
					((mathemadiga (INTEGRAL (exp1, (REAL(0.1 +. (mathemadiga (exp1)))), exp3))) +. (mathemadiga (INTEGRAL ((REAL(0.1 +. (mathemadiga (exp1)))), exp2, exp3))))
				else if (float02 < float01) then ( (mathemadiga (INTEGRAL (exp2, exp1, exp3))) *. (- 1.0))				
				else raise (Error exp00)
				end
			| INT(int02) -> (mathemadiga (INTEGRAL (exp1, (REAL (mathemadiga (exp2))), exp3)))
			| _ -> raise (Error exp00)
			end
		| _ -> raise (Error exp00)
		end
		