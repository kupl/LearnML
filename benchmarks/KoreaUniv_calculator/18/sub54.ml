type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;
  
  
(*NOT FINISHED, TIPS AND FEEDBACK ON HOW TO SOLVE IT ARE HIGHLY WELCOME. 
I got stuck on keeping track of the variables when evaluation Sigma.*)
let rec calculator : exp -> int
= fun exp -> 
  match exp with 
    | X ->  let x = pop_env in let env' = extend_env x v1 env in eval e2 env';;
    | INT n -> n
    | ADD (exp1, exp2) -> calculator(exp1) + calculator(exp2)
    | SUB (exp1, exp2) -> calculator(exp1) - calculator(exp2)
    | MUL (exp1, exp2) -> calculator(exp1) * calculator(exp2)
    | DIV (exp1, exp2) -> calculator(exp1) / calculator(exp2)
    | SIGMA (exp1, exp2, exp3) -> 
      let start = calculator(exp1) in
         let nums = calculator(exp2) in
            let rec expr st nm res = 
              match nm with 
(*                | 0 -> res*)
(*                | _ -> 0 *)
                in expr start nums 0 ;;
    
calculator (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)));;
