exception FreevarError
exception DividedByZero
exception Notatype
exception Rangeerror
type exp = X 
	| INT of int 
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp *exp
	| INTEGRAL of exp * exp * exp
type myenv = exp list

let rec solvesigandint : exp -> myenv -> float =
	(function thex ->
		(function thenv ->
			(match thex with 
				SIGMA (sta, las, someexp) ->
					(match (sta,las) with
						(INT alpha, INT beta) -> (if (alpha > beta) then raise Rangeerror
							else if (alpha < beta) then 
							(helper someexp (sta :: thenv)) +. (solvesigandint (SIGMA (INT (alpha+1),las, someexp)) thenv)
								else (helper someexp (las :: thenv)))
						| (_,_) -> raise Notatype)
				|INTEGRAL (sta, las, someexp)->
					let (alpha, beta) = ((helper sta thenv),(helper las thenv)) in	
						(if (alpha > beta) then 
							(-1.0 *. (solvesigandint (INTEGRAL (las, sta, someexp)) thenv))
							else if ((beta -. alpha) > 0.1) then
								(0.1 *. (helper someexp (sta :: thenv))) +.  
								(solvesigandint (INTEGRAL (REAL (alpha +. 0.1),las, someexp)) thenv)
								else 
								((beta -. alpha) *. (helper someexp (sta :: thenv)))   
								)
				| _ -> raise Notatype)))

and
helper : exp -> myenv -> float=
	(function thex ->
		(function thenv ->
			(match thex with
				INT mint -> (float_of_int mint)
				| REAL mfloat -> mfloat
				| ADD (exp1,exp2) -> (helper exp1 thenv) +. (helper exp2 thenv)
				| SUB (exp1,exp2) -> (helper exp1 thenv) -. (helper exp2 thenv)
				| MUL (exp1,exp2) -> (helper exp1 thenv) *. (helper exp2 thenv)
				| DIV (exp1,exp2) -> let val2 = (helper exp2 thenv) in
							if (val2 = 0.0) then raise DividedByZero
							else (helper exp1 thenv) /. (helper exp2 thenv)
				| SIGMA (sta, las, someexp) -> (solvesigandint thex thenv)
				| INTEGRAL (sta,las, someexp) -> (solvesigandint thex thenv)
				| X -> (match thenv with
						[] -> raise FreevarError
						| (h::t) -> (helper h t)))))
let mathemadiga : exp -> float =
	(function a ->
		(helper a []));;
