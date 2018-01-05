(* 2008-11874 Lee, Sujee *)
(* EXERCISE 2 *)

type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception CANNOTCAL of string 

let rec mathemadiga exp =
	let rec calculX(x,subex) =
			match subex with
				| X -> REAL x
				| INT i -> REAL (float_of_int i)
				| REAL r -> REAL r
				| ADD(e1,e2) -> ADD((calculX(x,e1)),(calculX(x,e2)))
				| SUB(e1,e2) -> SUB((calculX(x,e1)),(calculX(x,e2)))
				| MUL(e1,e2) -> MUL((calculX(x,e1)),(calculX(x,e2)))
				| DIV(e1,e2) -> DIV((calculX(x,e1)),(calculX(x,e2)))
				| SIGMA(e1,e2,ex) -> SIGMA((calculX(x,e1)),(calculX(x,e2)),ex)
				| INTEGRAL(e1,e2,ex) -> INTEGRAL((calculX(x,e1)),(calculX(x,e2)),ex)
	in
	
	match exp with
		| INT i -> float_of_int i
		| REAL r  -> r
		(*| ADD(REAL r1, REAL r2) -> r1 +. r2*)
		| ADD(ex1, ex2) -> (mathemadiga ex1) +. (mathemadiga ex2)
		(*| ADD(REAL r, ex) | ADD(ex, REAL r) -> r +. (mathemadiga ex) *)
		(*| SUB(REAL r1, REAL r2) -> r1 -. r2*)
		| SUB(ex1, ex2) -> (mathemadiga ex1) -. (mathemadiga ex2)
		(*| SUB(REAL r1, ex) -> r1 -. (mathemadiga ex)
		| SUB(ex, REAL r2) -> (mathemadiga ex) -. r2*)
		(*| MUL(REAL r1, REAL r2) -> r1 *. r2*)
		| MUL(ex1, ex2) -> (mathemadiga ex1) *. (mathemadiga ex2)
		(*| DIV(REAL r1, REAL r2) -> if r2=0.0 then raise (CANNOTCAL "devide by zero") else r1 /. r2*)
		| DIV(ex1, ex2) -> if (mathemadiga ex2)=0.0 then raise (CANNOTCAL "devide by zero") 
											else (mathemadiga ex1) /. (mathemadiga ex2)
												
		| SIGMA(INT i, INT j, subex) -> 
			if i=j then (mathemadiga (calculX((float_of_int j),subex)))
			else if i<j then ((mathemadiga (calculX((float_of_int j),subex))) +. (mathemadiga (SIGMA(INT i,INT (j-1),subex))))
			else 0.0
		| SIGMA(e1,e2,subex) -> mathemadiga (SIGMA(INT (int_of_float (mathemadiga e1)),INT (int_of_float (mathemadiga e2)),subex))
		| INTEGRAL(REAL r1, REAL r2, subex)  -> 
			if r1=r2 then 0.0
			else if (r1+.0.1)<=r2 then ((0.1*.(mathemadiga(calculX(r1,subex)))) +. (mathemadiga (INTEGRAL(REAL (r1 +. 0.1),REAL r2,subex))))
			else if (r1<r2) && (r1+.0.1>r2) then (r2 -. r1)*.(mathemadiga (calculX(r1,subex)))
			else if r1>r2 then (0.0 -. mathemadiga (INTEGRAL(REAL r2, REAL r1, subex)))
			else 0.0
		| INTEGRAL(e1,e2,subex) -> mathemadiga (INTEGRAL(REAL (mathemadiga e1),REAL (mathemadiga e2),subex))
		| X  -> raise (CANNOTCAL "freevar.")
	