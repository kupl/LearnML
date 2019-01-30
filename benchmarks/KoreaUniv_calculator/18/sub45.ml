type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> 
  match exp with
  |X->0
  |INT n->n
  |ADD(x,y)->(calculator x)+(calculator y)
  |SUB(x,y)->(calculator x)-(calculator y)
  |MUL(x,y)->(calculator x)*(calculator y)
  |DIV(x,y)->(calculator x)/(calculator y)
  |SIGMA(x,y,z)->sigma (calculator x,calculator y,z) 0 
and sigma : (int*int*exp)->int->int
= fun (x,y,z) sum ->
  if x>y then sum
  else sigma ((x+1),y,z) (sum+func z x)
and func : exp -> int -> int
= fun exp n->
  match exp with
    |X->n
    |INT k->k
    |ADD(x,y)->(func x n)+(func y n)
    |SUB(x,y)->(func x n)-(func y n)
    |MUL(x,y)->(func x n)*(func y n)
    |DIV(x,y)->(func x n)/(func y n)
    |SIGMA(x,y,z)->sigma (func x n,func y n,z) 0;;
  