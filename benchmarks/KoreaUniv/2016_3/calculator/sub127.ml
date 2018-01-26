
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec change: exp -> int -> exp
  = fun exp a->
  	match exp with
  		| X -> INT a
  		| INT(b) ->INT(b)
  		| ADD(b,c) -> ADD((change b a), (change c a))
  		| SUB(b,c) ->SUB((change b a), (change c a))
  		| MUL(b,c) -> MUL((change b a), (change c a))
  		| DIV(b,c) -> DIV((change b a), (change c a))
  		| SIGMA(b,c,d) -> SIGMA((change b a), (change c a), d)


  let rec calculator : exp -> int
  = fun exp -> 
  	match exp with 
  		| X -> raise NotImplemented
  		| INT(a) -> a
  		| ADD(a,b) -> calculator(a) + calculator(b)
  		| SUB(a,b) -> calculator(a) - calculator(b)
  		| MUL(a,b) -> calculator(a) * calculator(b)
  		| DIV(a,b) -> calculator(a) / calculator(b)
  		| SIGMA(a,b,c) -> if (calculator a > calculator b) then 0 
  						  else calculator(change c (calculator a)) + calculator(SIGMA(ADD(a,INT(1)),b,c))
