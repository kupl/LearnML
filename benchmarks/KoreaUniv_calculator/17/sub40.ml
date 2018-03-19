(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> let c = ref 0 in let rec calf fx =
match fx with
|X -> !c
|INT(i) -> i
|ADD(a,b) -> (calf a) + (calf b)
|SUB(a,b) -> (calf a) - (calf b)
|MUL(a,b) -> (calf a) * (calf b)
|DIV(a,b) -> (calf a) / (calf b)
|SIGMA(n1,n2,x) -> let rec sum(n1,n2,x) = c:=n1; if n1<=n2 then sum(n1+1,n2,x) + calf x else 0 in sum(calf n1, calf n2, x) in calf e
