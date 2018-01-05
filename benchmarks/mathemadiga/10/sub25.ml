exception DividedByZero
exception FreevarError
exception WrongInput
type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp

let rec mathemadiga e =
		let rec mathemadiga_with_x e x=
		match e with
		  X -> x
		| INT a -> (float)a
		| REAL a -> a
		| ADD (e1,e2) ->(mathemadiga_with_x e1 x) +. (mathemadiga_with_x e2 x)
		| SUB (e1,e2) ->(mathemadiga_with_x e1 x) -. (mathemadiga_with_x e2 x)
		| MUL (e1,e2) ->(mathemadiga_with_x e1 x) *. (mathemadiga_with_x e2 x)
		| DIV (e1,e2) -> if((mathemadiga_with_x e2 x)=0.0) then raise DividedByZero
     	else ((mathemadiga_with_x e1 x)/.(mathemadiga_with_x e2 x))
		| SIGMA (INT e1, INT e2, e3) -> if(e1=e2) then (mathemadiga_with_x e3 ((float)e1))
            else (mathemadiga_with_x e3 ((float)e1)) +. (mathemadiga_with_x (SIGMA (INT (e1 + 1),(INT e2), e3)) x)
		| INTEGRAL (e1, e2, e3) ->
		 if((mathemadiga_with_x e1 x) > (mathemadiga_with_x e2 x)) then (0.0 -. (mathemadiga_with_x (INTEGRAL (e2, e1, e3)) x))
            else if(((mathemadiga_with_x e1 x) +. 0.1) >= (mathemadiga_with_x e2 x)) then
            ((mathemadiga_with_x e2 x)-.(mathemadiga_with_x e1 x))*.(mathemadiga_with_x e3 (mathemadiga_with_x e1 x))
            else 0.1 *. (mathemadiga_with_x e3 (mathemadiga_with_x e1 x)) +. (mathemadiga_with_x (INTEGRAL (REAL ((mathemadiga_with_x e1 x)+.0.1), e2, e3)) x)
		| _ -> raise WrongInput in
		
		match e with
		X -> raise FreevarError
		| INT a -> (float)a 
		| REAL a -> a
		| ADD (e1,e2) -> (mathemadiga e1) +. (mathemadiga e2) 
		| SUB (e1,e2) -> (mathemadiga e1) -. (mathemadiga e2)
		| MUL (e1,e2) -> (mathemadiga e1) *. (mathemadiga e2)
		| DIV (e1,e2) -> 
		 if((mathemadiga e2)=0.0) then raise DividedByZero
		else (mathemadiga e1) /. (mathemadiga e2)
		| SIGMA (INT e1,INT e2,e3) -> 
			if(e1=e2) then (mathemadiga_with_x e3 ((float)e1))
	    	else (mathemadiga_with_x e3 ((float)e1)) +. (mathemadiga (SIGMA (INT (e1 + 1),(INT e2), e3)))
		| INTEGRAL (e1, e2, e3) ->
			if((mathemadiga e1) > (mathemadiga e2)) then (0.0 -. (mathemadiga (INTEGRAL (e2, e1, e3))))
			else if(((mathemadiga e1) +. 0.1) >= (mathemadiga e2)) then
            ((mathemadiga e2)-.(mathemadiga e1))*.(mathemadiga_with_x e3 (mathemadiga e1))
            else 0.1 *. (mathemadiga_with_x e3 (mathemadiga e1)) +. (mathemadiga (INTEGRAL (REAL ((mathemadiga e1)+.0.1), e2, e3)))
		| _ -> raise WrongInput  
