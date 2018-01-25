type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

exception FreeVariable;;

let rec calculator exp  = match exp with
  | X -> raise FreeVariable
  | INT (n) -> n
  | ADD (a,b) -> (calculator a) + (calculator b)
  | SUB (a,b) -> (calculator a) - (calculator b) 
  | MUL (a,b) -> (calculator a) * (calculator b) 
  | DIV (a,b) -> (calculator a) / (calculator b) 
  | SIGMA (a,b,f) -> if (calculator a) > (calculator b) then 0
                        else cal (f,a) + calculator (SIGMA (ADD(a,INT 1),b,f))
  and cal (f,a) = match f with
     | X -> calculator a
     | INT n -> n 
     | ADD (f1,f2) -> cal(f1,a) + cal(f2,a)
     | SUB (f1,f2) -> cal(f1,a) - cal(f2,a)
     | MUL  (f1,f2) -> cal(f1,a) * cal(f2,a)
     | DIV  (f1,f2) -> cal(f1,a) / cal(f2,a)
     | SIGMA (c,d,f) -> if (calculator (c) > calculator (d)) then 0 
                        else if (calculator (c) > calculator (d)) then cal (f,c)
                        else (cal (f,c) + calculator ( SIGMA (ADD (INT (calculator c),INT 1),d,f)));;
