type var = string

type exp =
  | V of var
  | P of var * exp
  | C of exp * exp

  let check : exp -> bool
  = fun exp -> true;;