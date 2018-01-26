(* problem 5*)
let rec nth l n =
match l with
  [] -> 0
  |a::t -> if n=0 then a else nth t (n-1)

type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e ->
let rec evalC env sum exp =
match exp with
  |X -> nth env 0
  |INT a -> a
  |ADD(a, b) -> evalC env sum a + evalC env sum b
  |SUB(a, b) -> evalC env sum a - evalC env sum b
  |MUL(a, b) -> evalC env sum a * evalC env sum b
  |DIV(a, b) -> evalC env sum a / evalC env sum b
  |SIGMA(a,b,c) -> if (evalC env sum a) > (evalC env sum b) then sum else evalC ((evalC env sum a)::env) (sum + (evalC ((evalC env sum a)::env) sum c)) (SIGMA(INT ((evalC env sum a)+1), b, c))
in evalC [] 0 e