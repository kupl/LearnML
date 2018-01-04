exception ERROR of string
type nat = ZERO | SUCC of nat
let rec natadd (n1,n2) = 
	match n2 with ZERO -> n1
		|SUCC na -> natadd (SUCC n1, na)
let rec natsub (n1,n2) =
	match (n1,n2) with (_ ,ZERO) -> n1
		|(ZERO , _) -> raise (ERROR "first number should bigger than second number")
		|(SUCC n1, SUCC n2) -> natsub (n1 , n2)
