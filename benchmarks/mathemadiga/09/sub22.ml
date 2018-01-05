(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_second_homework\HW5_Exercise5.ml *)

(*Exercise 5*)
type exp = X
	| INT of int
	| REAL of float
	| ADD of exp*exp
	| SUB of exp*exp 
	| MUL of exp*exp
	| DIV of exp*exp
	| SIGMA of exp*exp*exp
	| INTEGRAL of exp*exp*exp 

exception InvalidSigma
exception DivideByZero
exception FreeVariable
		;;

let rec mathemadiga expr =
	let rec f1_help (a,b,z,s,p) =
	match z with
	|X-> (  if p=1 then
		f1_help (X,X,a,s,p)
		else raise FreeVariable
	      )	
	|INT x -> (float_of_int x)
	|REAL x -> x
	|ADD (x,y) -> (f1_help (a,b,x,s,p))+.(f1_help (a,b,y,s,p))
	|SUB (x,y) -> (f1_help (a,b,x,s,p))-.(f1_help (a,b,y,s,p))
	|MUL (x,y) -> (f1_help (a,b,x,s,p))*.(f1_help (a,b,y,s,p))
	|DIV (x,y) -> ( if (y=(INT 0)||y=(REAL 0.)) then (raise DivideByZero)
			else (f1_help (a,b,x,s,p))/.(f1_help (a,b,y,s,p))
		       )
	|SIGMA (x,y,z) ->  (
				if (f1_help (X,X,x,s,p))<(f1_help (X,X,y,s,p)) 
				then (f1_help (x,y,z,s,1))+.(f1_help (x,y,SIGMA (ADD (x, INT 1),y,z),s,1) )
				else if (f1_help (X,X,x,s,1))=(f1_help (X,X,y,s,1))
				then f1_help (x,y,z,s,1)
				else raise InvalidSigma
					)
	|INTEGRAL (x,y,z) -> (
				if (f1_help (X,X,x,s,p))<(f1_help (X,X,y,s,p)) && (s=3 || s=1) 
				then 0.1*.(f1_help (x,y,z,1,1))+.(f1_help (x,y,INTEGRAL (ADD (x, REAL 0.1),y,z),1,1) )
				else if (f1_help (X,X,x,s,p))>(f1_help (X,X,y,s,p)) && s=1
				then 0.
				else if (f1_help (X,X,x,s,p))>(f1_help (X,X,y,s,p)) && (s=3 || s=0) 
				then ((-0.1)*.(f1_help (x,y,z,0,1)))+.(f1_help (x,y,INTEGRAL (SUB (x, REAL 0.1),y,z),0,1) )
				else 0.
				
			     )
	
	  in
		f1_help (X,X,expr,3,0)
;;

