type x = Nil | Number of float

and exp = X
         | INT of int
	 | REAL of float
	 | ADD of exp * exp
	 | SUB of exp * exp
	 | MUL of exp * exp
	 | DIV of exp * exp
	 | SIGMA of exp * exp * exp
	 | INTEGRAL of exp * exp * exp

exception FreeVariable

let rec calc (exp:exp) (x:x) :float =
  match exp with
  | X -> (match x with
         | Nil -> raise FreeVariable
	 | Number r -> r)
  | INT i -> float i
  | REAL r -> r
  | ADD (e1, e2) -> (calc e1 x) +. (calc e2 x) 
  | SUB (e1, e2) -> (calc e1 x) -. (calc e2 x)
  | MUL (e1, e2) -> (calc e1 x) *. (calc e2 x)
  | DIV (e1, e2) -> (calc e1 x) /. (calc e2 x)
  | SIGMA (e1, e2, e3) -> (sigma (calc e1 Nil) (calc e2 Nil) e3)
  | INTEGRAL (e1, e2, e3) -> (integral (calc e1 Nil) (calc e2 Nil) e3)

and sigma (a:float) (b:float) (exp:exp) :float =
  let rec iter a b exp result =
    if a > b then result
    else (iter (a +. 1.0) b exp (result +. (calc exp (Number a))))
  in
  iter a b exp 0.0

and integral (a:float) (b:float) (exp:exp) :float =
  let rec iter a b exp result =
    if a > b then result
    else (iter (a +. 0.1) b exp (result +. (0.1 *. (calc exp (Number a)))))
  in
  if a > b then (iter a b exp 0.0)
  else (-1.0 *. (iter b a exp 0.0))


let galculator exp = calc exp Nil
