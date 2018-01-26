
exception NONONO

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec modulation : exp * int -> int
= fun (e,num) ->
match e with
	| X -> num
	| INT ( n ) -> n
	| ADD(e1,e2) -> modulation(e1,num)+modulation(e2,num)
	| SUB(e1,e2) -> modulation(e1,num)-modulation(e2,num)
	| MUL(e1,e2) -> modulation(e1,num)*modulation(e2,num)
	| DIV(e1,e2) -> modulation(e1,num)/modulation(e2,num)
	| SIGMA(e1,e2,e3) -> 
		if modulation(e1,num)>modulation(e2,num) then raise NONONO
		else if modulation(e1,num)=modulation(e2,num) then modulation(e3,modulation(e1,num))
		else modulation(e3,modulation(e1,num))+
		modulation ( SIGMA ( INT(modulation(e1,num)+1) , INT (modulation(e2,num)), e3), num)

  and calculator : exp -> int
  = fun exp -> 
  		match exp with
  		 | X -> raise NONONO
  		 | INT(n) -> n
  		 | ADD(e1,e2) -> calculator(e1) + calculator(e2)
  		 | SUB(e1,e2) -> calculator(e1) - calculator(e2)
  		 | MUL(e1,e2) -> calculator(e1) * calculator(e2)
  		 | DIV(e1,e2) -> calculator(e1) / calculator(e2)
  		 | SIGMA(e1,e2,e3) -> 
  		     if calculator(e1) > calculator(e2) then raise NONONO
      		else if calculator(e1) = calculator(e2) then modulation(e3,calculator(e1))(*plus*)
      		else modulation(e3,calculator(e1)) + calculator(SIGMA(INT (calculator(e1) +1), INT (calculator(e2)), e3))
