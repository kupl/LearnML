
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec getVal : exp * int -> int
  = fun (ep, num) ->
	match ep with
	|X -> num
	|INT i -> i
        |ADD (x, y) -> (getVal (x, num)) + (getVal (y, num))
        |SUB (x, y) -> (getVal (x, num)) - (getVal (y, num))
	|MUL (x, y) -> (getVal (x, num)) * (getVal (y, num))
	|DIV (x, y) -> (getVal (x, num)) / (getVal (y, num))
	|_-> raise (Failure "notProper")

  let calculator : exp -> int
  = fun exp -> 
	match exp with
	|INT i -> i
	|ADD (x, y) -> (getVal (x, 0)) + (getVal (y, 0))
        |SUB (x, y) -> (getVal (x, 0)) - (getVal (y, 0))
        |MUL (x, y) -> (getVal (x, 0)) * (getVal (y, 0))
        |DIV (x, y) -> (getVal (x, 0)) / (getVal (y, 0))
	|SIGMA (f,t,body) -> raise (Failure "NotReadyforSigma")
	|_-> raise (Failure "notProper")	