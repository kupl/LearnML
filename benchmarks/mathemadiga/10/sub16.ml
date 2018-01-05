type exp = X
	|INT of int
	|REAL of float
	|ADD of exp * exp
	|SUB of exp * exp
	|MUL of exp * exp
	|DIV of exp * exp
	|SIGMA of exp * exp * exp
	|INTEGRAL of exp * exp * exp
exception FreevarError
exception DividedByZero
exception Error of string

let rec mathemadiga e = 
  match e with
    X -> raise FreevarError
    |INT a -> float(a) 
    |REAL a -> a
    |ADD (a,b) -> (mathemadiga a) +. (mathemadiga b)
    |SUB (a,b) -> (mathemadiga a) -. (mathemadiga b)
    |MUL (a,b) -> (mathemadiga a) *. (mathemadiga b)
    |DIV (a,b) -> 
      if ((mathemadiga b) = 0.) then raise DividedByZero
      else (mathemadiga a) /. (mathemadiga b)
    |SIGMA (INT a, INT b, c) -> (sigma a b c )
    |SIGMA (_,_,_) -> raise (Error "Sigma operation should be run with INT exp")
    |INTEGRAL (a,b,c) -> 
    (
      let st = (mathemadiga a) in
      let en = (mathemadiga b)  in
      if (st > en) then (integral en st c) *. -1.
      else (integral st en c)
    )
and eval e x = 
  match e with
    X -> x
    |INT a -> float(a)
    |REAL a -> a
    |ADD (a,b) -> (eval a x) +. (eval b x)
    |SUB (a,b) -> (eval a x) -. (eval b x)
    |MUL (a,b) -> (eval a x) *. (eval b x)
    |DIV (a,b) -> 
      if ((eval b x) = 0.) then raise DividedByZero
      else (eval a x) /. (eval b x)
    |SIGMA (INT a, INT b, c) -> (sigma a b c)
    |SIGMA (_,_,_) -> raise (Error "Sigma operation should be run with INT exp")
    |INTEGRAL (a,b,c) ->
    (
      let st = (eval a x) in
      let en = (eval b x)  in
      if (st > en) then (integral en st c) *. -1.
      else (integral st en c)
    )
and sigma start ending e =
  if (start > ending) then 0.
  else (eval e (float(start))) +. (sigma (start+1) ending e)
and integral start ending e = 
  if (start +. 0.1 >= ending) then (ending -. start) *. (eval e start)
  else 0.1 *. (eval e start) +. (integral (start +. 0.1) ending e)
