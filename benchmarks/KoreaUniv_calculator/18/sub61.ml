type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator exp
  = let rec helper exp xval = match exp with
    |X -> xval
    |INT x -> x
    |ADD (x, y) -> helper x xval + helper y xval
    |SUB (x, y) -> helper x xval - helper y xval
    |MUL (x, y) -> helper x xval * helper y xval
    |DIV (x, y) -> helper x xval / helper y xval
    |SIGMA (x, y, z) -> if (calculator x > calculator y) then 0 else ((helper z (calculator x)) + (helper ( SIGMA (INT ((calculator x)+1), y, z) )) xval)
    in helper exp 0;;