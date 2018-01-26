(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec xcalculator: int -> exp -> int
= fun n e -> match e with
           | X -> n
           | INT t -> t
           | ADD (X, X) -> n+n
           | ADD (X, INT t) -> n+t
           | ADD (INT t, X) -> n+t
           | ADD (INT t, INT u) -> t+u
           | ADD (e1, e2) -> (xcalculator n e1)+(xcalculator n e2)
           | SUB (X, X) -> 0
           | SUB (X, INT t) -> n-t
           | SUB (INT t, X) -> t-n
           | SUB (INT t, INT u) -> t-u
           | SUB (e1, e2) -> (xcalculator n e1)-(xcalculator n e2)
           | MUL (X, X) -> n*n
           | MUL (X, INT t) -> n*t
           | MUL (INT t, X) -> n*t
           | MUL (INT t, INT u) -> t*u
           | MUL (e1, e2) -> (xcalculator n e1)*(xcalculator n e2)
           | DIV (X, X) -> 1
           | DIV (X, INT t) -> n/t
           | DIV (INT t, X) -> t/n
           | DIV (INT t, INT u) -> t/u
           | DIV (e1, e2) -> (xcalculator n e1)/(xcalculator n e2)
           

let rec calculator : exp -> int
= fun e -> match e with
           | INT n -> n
           | ADD (e1,e2) -> (calculator e1)+(calculator e2)
           | SUB (e1,e2) -> (calculator e1)-(calculator e2)
           | MUL (e1,e2) -> (calculator e1)*(calculator e2)
           | DIV (e1,e2) -> (calculator e1)/(calculator e2)
           | SIGMA (INT e1,INT e2,e3) -> if e2<e1 then 0 else (xcalculator e1 e3) + calculator (SIGMA(INT (e1+1), INT e2, e3))
           | SIGMA (e1,e2,e3) -> calculator (SIGMA(INT (calculator e1), INT (calculator e2), e3))
