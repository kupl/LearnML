(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> let rec realcal e c = match e
with X -> c
| INT x -> x
| ADD (x, y) -> (realcal x c) + (realcal y c)
| SUB (x, y) -> (realcal x c) - (realcal y c)
| MUL (x, y) -> (realcal x c) * (realcal y c)
| DIV (x, y) -> (realcal x c) / (realcal y c)
| SIGMA (x, y, z) -> let b = realcal x c 
in let t = realcal y c 
in let rec sigma counter = if counter > t then 0 else (realcal z counter) + sigma (counter + 1)
in sigma b
in realcal e 0s