exception FreeVariable
exception DivideByZero
exception InvalidSigma
type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp


let rec mathemadiga ex=
	let rec makelist_sigma e1 e2 e3 =
		let rec x_apply e x =			
			match e with 
				| X ->  x
				| INT(i)->float_of_int i
				| REAL(f)->f
				| ADD(e1, e2)->(x_apply e1 x)+.(x_apply e2 x)
				| SUB(e1, e2)->(x_apply e1 x)-.(x_apply e2 x)
				| MUL(e1, e2)->(x_apply e1 x)*.(x_apply e2 x)
				| DIV(e1, e2)-> if (x_apply e2 x=0.0) then raise (DivideByZero)
											else (x_apply e1 x)/.(x_apply e2 x)
				|SIGMA(e1, e2, e3)-> (mathemadiga e)
				|INTEGRAL(e1, e2, e3)->(mathemadiga e)
		in 
		if (mathemadiga e1)>(mathemadiga e2) then []
		 
		else (x_apply e3 (mathemadiga e1) )::(makelist_sigma (REAL((mathemadiga e1)+.1.0)) e2 e3)
	in
	
	let rec makelist_integral e1 e2 e3 =
		let rec x_apply e x =			
			match e with 
				| X -> x
				| INT(i)->float_of_int i
				| REAL(f)->f
				| ADD(e1, e2)->(x_apply e1 x)+.(x_apply e2 x)
				| SUB(e1, e2)->(x_apply e1 x)-.(x_apply e2 x)
				| MUL(e1, e2)->(x_apply e1 x)*.(x_apply e2 x)
				| DIV(e1, e2)-> if (x_apply e2 x=0.0) then raise (DivideByZero)
											else (x_apply e1 x)/.(x_apply e2 x)
				|SIGMA(e1, e2, e3)-> (mathemadiga e)
				|INTEGRAL(e1, e2, e3)-> (mathemadiga e)
		in 
		if (mathemadiga e1)>=(mathemadiga e2) then []
		else if(mathemadiga e2)-.(mathemadiga e1)<0.1 then [((mathemadiga e2)-.(mathemadiga e1))/.0.1*.(x_apply e3 (mathemadiga e1))]
		else (x_apply e3 (mathemadiga e1) )::(makelist_integral (REAL((mathemadiga e1)+.0.1)) e2 e3)
	in
		
	
			
		  
	 	
	match ex with
		| X ->raise(FreeVariable)
		| INT(i)-> float_of_int i
		| REAL(f)->f
		| ADD(e1, e2)->(mathemadiga e1)+.(mathemadiga e2)
		| SUB(e1, e2)->(mathemadiga e1)-.(mathemadiga e2)
		| MUL(e1, e2)->(mathemadiga e1)*.(mathemadiga e2)
		| DIV(e1, e2)-> if (mathemadiga e2=0.0) then raise (DivideByZero)
										else (mathemadiga e1)/.(mathemadiga e2)
		|SIGMA(e1, e2, e3)-> if (mathemadiga e1)>(mathemadiga e2) then raise(InvalidSigma)
												else List.fold_left(fun x y->x+.y) 0.0 (makelist_sigma e1 e2 e3)
		|INTEGRAL(e1, e2, e3)->if (mathemadiga e1)>(mathemadiga e2) then mathemadiga(SUB(INT(-1), INTEGRAL(e2, e1, e3)))
													else 0.1*.(List.fold_left(fun x y->x+.y) 0.0 (makelist_integral e1 e2 e3))