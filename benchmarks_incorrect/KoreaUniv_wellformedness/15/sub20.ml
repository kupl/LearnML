type var = string
 type exp = Var of var
 | P of var * exp
 | C of exp * exp

let check : exp -> bool
=fun e -> true

