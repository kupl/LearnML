type var = string
 type exp = Var of var
 | P of var * exp
 | C of exp * exp
let id : exp = P ("x", Var "x")
 let w : exp = 
C (P ("x", C (Var "x", Var "x")), 
	P ("x", C (Var "x", Var "x")))
let check : exp -> bool
=fun e -> true

