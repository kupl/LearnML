type formula = True
              | False
              | Not of formula
              | AndAlso of formula * formula
              | OrElse of formula * formula
              | Imply of formula * formula
              | Equal of exp * exp 
and exp = Num of int
         | Plus of exp * exp
         | Minus of exp * exp
              
let rec eval f = 
  let rec cal a = 
    match a with
      | Num x -> x
      | Plus (x, y) -> cal x + cal y
      | Minus (x, y) -> cal x - cal y in
  match f with
  | True -> true
  | False -> false
  | Not g -> 
      if (eval g) = true then false
      else true
  | AndAlso (g, h) -> 
      if (eval g) = true & (eval h) = true then true
      else false
  | OrElse (g, h) -> 
      if (eval g) = false & (eval h) = false then false
      else true
  | Imply (g, h) -> 
      if (eval g) = false then true
      else if (eval h) = true then true
      else false
  | Equal (g, h) -> 
      if cal g = cal h then true
      else false
