type exp = X
				| INT of int
				| REAL of float
				| ADD of exp*exp
				| SUB of exp*exp
				| MUL of exp*exp
				| DIV of exp*exp
				| SIGMA of exp*exp*exp
				| INTEGRAL of exp*exp*exp

exception FreeVariable

let galculator (* : exp -> float *) exp = 
	let rec eval env = function (* : exp list -> exp -> float *)
		| X -> if env=[] then raise FreeVariable else eval (List.tl env) (List.hd env)
		| REAL r -> r
		| INT i -> float_of_int i
		| ADD(e1, e2) -> (eval env e1) +. (eval env e2)
		| SUB(e1, e2) -> (eval env e1) -. (eval env e2)
		| MUL(e1, e2) -> (eval env e1) *. (eval env e2)
		| DIV(e1, e2) -> (eval env e1) /. (eval env e2)
		| SIGMA( e1, e2, f ) -> 
				if ( eval env (SUB(e2,e1)) ) < 0.0 then 0.0
				else 
				(eval ((INT (int_of_float (eval env e1)))::env) f) +. (eval env ( SIGMA( ADD( e1, INT 1), e2, f ) ) )
		| INTEGRAL( e1, e2, f ) ->
				let diff = ( eval env (SUB(e2,e1)) ) in
					if diff < 0.0 then (eval env (INTEGRAL( e2, e1, f ))) *. (-1.0)
					else if diff < 0.1 && diff > ( -0.1 ) then 0.0
					else (eval (e1::env) (MUL( f, REAL 0.1 )) ) +. (eval env ( INTEGRAL( ADD( e1, REAL 0.1), e2, f )))

	in
	eval [] exp


(*
let testq1 = ADD( INT 1, REAL 2.5 )
let testq2 = SIGMA( INT 1, INT 10, MUL( X, X) )
let testq3 = SIGMA( INT 1, INT 10, ADD( MUL(X,X) , INT 1 ) )
let testq4 = SIGMA( INT 1, INT 10, ADD( SIGMA( INT 1, X, X ), X ) )
let testq5 = SIGMA( INT 1, INT 500, DIV(INT 8, MUL(SUB(MUL(INT 2, X), INT 1), SUB(MUL(INT 2, X), INT 1))))
let testq6 = SIGMA( INT 1, INT 500, X )
let testq7 = SIGMA( INT 1, INT 20, SIGMA( INT 1, MUL(X,X), X) )
let testq8 = INTEGRAL( INT 0, INT 2, MUL(X,X) )
let testq9 = INTEGRAL( INT 0, INT 10, X )

let _= print_string "testq2 : "; print_float ( galculator testq2 ); print_string "\n";;
let _= print_string "testq4 : "; print_float ( galculator testq4 ); print_string "\n";;
let _= print_string "testq7 : "; print_float ( galculator testq7 ); print_string "\n";;
*)
(*
let _= print_string "testq1 : "; print_float ( galculator testq1 ); print_string "\n";;
let _= print_string "testq2 : "; print_float ( galculator testq2 ); print_string "\n";;
let _= print_string "testq3 : "; print_float ( galculator testq3 ); print_string "\n";;
let _= print_string "testq4 : "; print_float ( galculator testq4 ); print_string "\n";;
let _= print_string "testq5 : "; print_float ( galculator testq5 ); print_string "\n";;
let _= print_string "testq6 : "; print_float ( galculator testq6 ); print_string "\n";;
let _= print_string "testq7 : "; print_float ( galculator testq7 ); print_string "\n";;
let _= print_string "testq8 : "; print_float ( galculator testq8 ); print_string "\n";;
let _= print_string "testq9 : "; print_float ( galculator testq9 ); print_string "\n";;
*)
