type nat = ZERO | SUCC of nat

let rec  natadd (n1,n2) = 
	match (n1,n2) with
	| (SUCC x,SUCC _) -> (SUCC (natadd (x,n2)))
	| (ZERO, SUCC x) -> (SUCC (natadd (ZERO,x)))
	| (SUCC x, ZERO) -> (SUCC (natadd (ZERO,x)))
	| (ZERO, ZERO) -> ZERO

let rec natmul (n1,n2) = 
	match (n1,n2) with
	| (SUCC _ ,SUCC y) -> (natadd (n1 ,(natmul (n1,y))))
	| (SUCC _ ,ZERO) -> ZERO
	| (ZERO , SUCC _) -> ZERO
	| (ZERO , ZERO) -> ZERO
