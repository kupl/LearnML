type nat = ZERO 
         | SUCC of nat
 
let rec natadd (natf, natl) = 
	match (natf, natl) with
	|(ZERO, _) -> natl
	|(SUCC (x), _) -> natadd (x, SUCC (natl))
	
let rec natmul (natf, natl) =
	match (natf, natl) with
	|(ZERO, _) -> ZERO
	|(_, ZERO) -> ZERO
	|(SUCC (ZERO), _) -> natl 
	|(SUCC (SUCC (x)), _) -> natadd (natl, natmul(SUCC (x), natl))

