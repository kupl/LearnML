exception IllegalInput
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

	let rec change : exp * int -> int
	= fun (e,num) ->
	match e with
	| X -> num
	| INT (n) -> n
	| ADD(e1,e2) -> change(e1,num)+change(e2,num)
	| SUB(e1,e2) -> change(e1,num)-change(e2,num)
	| MUL(e1,e2) -> change(e1,num)*change(e2,num)
	| DIV(e1,e2) -> change(e1,num)/change(e2,num)
	| SIGMA(e1,e2,e3) ->
		if change(e1,num)>change(e2,num) then raise IllegalInput
		else if change(e1,num)=change(e2,num) then change(e3,change(e1,num))
		else change(e3,change(e1,num))+change(SIGMA(INT (change(e1,num)+1), INT (change(e2,num)), e3),num)

	and calculator : exp -> int
  = fun exp -> (* TODO *)
	match exp with
	| X -> raise IllegalInput
	| INT(n) -> n
	| ADD(e1,e2) -> calculator(e1)+calculator(e2)
	| SUB(e1,e2) -> calculator(e1)-calculator(e2)
	| MUL(e1,e2) -> calculator(e1)*calculator(e2)
	| DIV(e1,e2) -> calculator(e1)/calculator(e2)
	| SIGMA(e1,e2,e3) ->
		if calculator(e1)>calculator(e2) then raise IllegalInput
		else if calculator(e1)=calculator(e2) then change(e3,calculator(e1))
		else change(e3,calculator(e1))+calculator(SIGMA(INT (calculator(e1) +1), INT (calculator(e2)), e3))
