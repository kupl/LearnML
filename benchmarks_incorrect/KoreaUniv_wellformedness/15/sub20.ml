type var = string
 type lambda = Var of var
 | P of var * lambda
 | C of lambda * lambda

let check : lambda -> bool
=fun e -> true

