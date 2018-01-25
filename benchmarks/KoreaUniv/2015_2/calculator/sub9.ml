type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp;;

let rec calculator : exp -> int
=fun e -> (match e with
INT a -> a |
ADD (a,b) -> calculator a + calculator b |
SUB (a,b) -> calculator a - calculator b |
MUL (a,b) -> calculator a * calculator b |
DIV (a,b) -> calculator a / calculator b |
SIGMA (a,b,c) -> if a=b then calculator(foo(c,a)) else
  (calculator(foo(c,a)) + calculator(SIGMA(INT(calculator(ADD(a,INT 1))),b,c))))

and foo : exp * exp -> exp
=fun (c,x) -> match c with
INT a -> INT a |
ADD(a,b) -> ADD(foo(a,x),foo(b,x)) |
SUB(a,b) -> SUB(foo(a,x),foo(b,x)) |
MUL(a,b) -> MUL(foo(a,x),foo(b,x)) |
DIV(a,b) -> DIV(foo(a,x),foo(b,x)) |
X -> x |
SIGMA (a,b,c) -> SIGMA(a,b,c);;