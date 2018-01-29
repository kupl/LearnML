(*problem 5*)
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e ->
let rec cal e num = 
match e with
| X -> num
| INT a -> a
| ADD (x, y) -> (cal x num) + (cal y num)
| SUB (x, y) -> (cal x num) - (cal y num)  
| MUL (fst, snd) -> (cal fst num) * (cal snd num)
| DIV (fst, snd) -> (cal fst num) / (cal snd num)
| SIGMA(s, t, c) -> 
let k = (cal s num) 
in let m = (cal t num)
in let rec sum k =
if k > m then 0
else (cal c k) + sum(k + 1) in sum k in cal e 0;;