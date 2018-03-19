(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calc_env : exp -> int -> int
= fun e num -> (match e with
           | X -> num
           | INT num1 -> num1
           | ADD (exp1, exp2) -> ((calc_env exp1 num)+(calc_env exp2 num))
           | SUB (exp1, exp2) -> ((calc_env exp1 num)-(calc_env exp2 num))
           | MUL (exp1, exp2) -> ((calc_env exp1 num)*(calc_env exp2 num))
           | DIV (exp1, exp2) -> ((calc_env exp1 num)/(calc_env exp2 num))
           | SIGMA (exp1, exp2, exp3) -> let v1 = (calc_env exp1 num) in
                                         let v2 = (calc_env exp2 num) in
                                         if (v1=v2) then (calc_env exp3 v1)
                                         else ((calc_env exp3 v1)+(calc_env (SIGMA ((INT (v1+1)), exp2, exp3)) v1))
            )

let calculator : exp -> int
= fun e -> (calc_env e 0)