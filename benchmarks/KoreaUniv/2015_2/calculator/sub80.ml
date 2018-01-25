type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator input =
 match input with
INT i -> i
| ADD (a,b) -> calculator(a) + calculator(b)
| SUB (a,b) -> calculator(a) - calculator(b)
| MUL (a,b) -> calculator(a) * calculator(b)
| DIV (a,b) -> calculator(a) / calculator(b)
| SIGMA (a,b,c) -> if (calculator(a))<=(calculator(b))
                   then func1 (calculator(a)) c +
                        calculator(SIGMA(INT( calculator(ADD(a,INT(1))) ),  b,  c ))
                   else 0
(*func1 : int,exp -> int*)
and func1 a c =
 match c with
INT i -> i
| ADD(a1,a2) -> if a1=X && a2=X then a+a
              else if a1=X && a2<>X then a+func1 a a2
              else if a1<>X && a2=X then func1 a a1+a
              else func1 a a1+func1 a a2
| SUB(a1,a2) -> if a1=X && a2=X then 0
                else if a1=X && a2<>X then a-func1 a a2
                else if a1<>X && a2=X then func1 a a1-a
                else func1 a a1-func1 a a2
| MUL(a1,a2) -> if a1=X && a2=X then a*a
                else if a1=X && a2<>X then a*func1 a a2
                else if a1<>X && a2=X then func1 a a1*a
                else func1 a a1*func1 a a2
| DIV(a1,a2) -> if a1=X && a2=X then a/a
                else if a1=X && a2<>X then a/func1 a a2
                else if a1<>X && a2=X then func1 a a1/a
                else func1 a a1/func1 a a2;;

