
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

	exception No_Value
	let rec eval : exp * exp -> int
	= fun (exp, num) ->
	match exp with
	X -> eval(num, num)
	|INT a -> a
	|ADD(a,b) -> eval(a,num) + eval(b, num)
	|SUB(a,b) -> eval(a,num) - eval(b, num)
	|MUL(a,b) -> eval(a,num) * eval(b, num)
	|DIV(a,b) -> eval(a,num) / eval(b, num)
	|SIGMA (a,b,c) -> if eval(X,a) = eval(X,b) then eval (c,a) else eval(SIGMA(a, SUB(b, INT 1), c), INT 0) + eval(c, b)
  let calculator : exp -> int
  = fun exp -> if exp = X then raise No_Value else eval(exp,X)(* TODO *)