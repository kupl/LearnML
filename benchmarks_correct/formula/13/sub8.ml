type formula = TRUE
              | FALSE
              | NOT of formula
              | ANDALSO of formula * formula
              | ORELSE of formula * formula
              | IMPLY of formula * formula
              | LESS of expr * expr 
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr
              
let rec eval f = 
  let rec cal a = 
    match a with
      | NUM x -> x
      | PLUS (x, y) -> cal x + cal y
      | MINUS (x, y) -> cal x - cal y in
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT g -> 
      if (eval g) = true then false
      else true
  | ANDALSO (g, h) -> 
      if (eval g) = true & (eval h) = true then true
      else false
  | ORELSE (g, h) -> 
      if (eval g) = false & (eval h) = false then false
      else true
  | IMPLY (g, h) -> 
      if (eval g) = false then true
      else if (eval h) = true then true
      else false
  | LESS (g, h) -> 
      if cal g < cal h then true
      else false
