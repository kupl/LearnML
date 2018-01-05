type nat = ZERO | SUCC of nat

let rec natadd (x, y) = match (x,y) with
						|(ZERO, nat1)
						|(nat1, ZERO) -> nat1
						|(SUCC nat2, SUCC nat3) -> SUCC (SUCC (natadd(nat2, nat3)))

let rec natmul (x, y) = match (x,y) with
						|(ZERO, nat1)
						|(nat1, ZERO) -> ZERO
						|(SUCC nat2, nat3) -> natadd(nat3, (natmul(nat2, nat3))) 
