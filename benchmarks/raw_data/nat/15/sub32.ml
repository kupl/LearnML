type nat = ZERO | SUCC of nat

let rec natadd (a, b)  =
	match (a, b) with
 	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC (xval)) -> a
  | (SUCC (xval), ZERO) -> b
 	| (SUCC (yval), SUCC (zval)) -> SUCC( SUCC( natadd (yval, zval)))

let rec natmul (c, d) =
 	match (c, d) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC (eval)) -> ZERO
	| (SUCC (eval), ZERO) -> ZERO
	| (SUCC (fval), SUCC (gval)) -> natadd( natmul (fval, d), d)

