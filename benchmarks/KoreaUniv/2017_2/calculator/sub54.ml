(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec intprt f i= match f with
  |X->INT i
  |INT j->INT j
  |ADD (p,q)->ADD (intprt p i, intprt q i)
  |SUB (p,q)->SUB (intprt p i, intprt q i)
  |MUL (p,q)->MUL (intprt p i, intprt q i)
  |DIV (p,q)->DIV (intprt p i, intprt q i)

let rec calculator : exp -> int
= fun e ->match e with
  |INT i->i
  |ADD (p,q)->calculator p + calculator q
  |SUB (p,q)->calculator p - calculator q
  |MUL (p,q)->(calculator p)*(calculator q)
  |DIV (p,q)->(calculator p)/(calculator q)
  |SIGMA (i,j,p)->if calculator i>calculator j then 0
           else calculator (ADD(intprt p (calculator i),SIGMA (ADD (i,INT 1),j,p)))
