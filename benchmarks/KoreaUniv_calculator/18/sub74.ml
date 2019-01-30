type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec eval_sigma: exp -> int -> int -> int
 =fun exp x y -> if x>y then 0
                 else (match exp with
                       X -> x
                      |INT n -> n
                      |ADD(exp1, exp2) -> let a = eval_sigma exp1 x y
                                          in let b = eval_sigma exp2 x y
                                          in a+b
                      |SUB(exp1, exp2) -> let a = eval_sigma exp1 x y
                                          in let b = eval_sigma exp2 x y
                                          in a-b
                      |MUL(exp1, exp2) -> let a = eval_sigma exp1 x y
                                          in let b = eval_sigma exp2 x y
                                          in a*b
                      |DIV(exp1, exp2) -> let a = eval_sigma exp1 x y
                                          in let b = eval_sigma exp2 x y
                                          in a/b
                      |SIGMA(exp1, exp2, exp3) -> let a = eval_sigma exp1 x y
                                                  in let b = eval_sigma exp2 x y
                                                  in eval_sigma exp3 a b);;
let rec sigma: exp->int->int->int
 = fun exp a b -> if a > b then 0
                    else (eval_sigma exp a b) + (sigma exp (a+1) b);;


let rec eval_num :exp -> int
 =fun exp -> match exp with
   INT n -> n
  |ADD(exp1, exp2) -> let a = eval_num exp1
                      in let b = eval_num exp2
                      in a+b
  |SUB(exp1, exp2) -> let a = eval_num exp1
                      in let b = eval_num exp2
                      in a - b
  |MUL(exp1, exp2) -> let a = eval_num exp1
                      in let b = eval_num exp2
                      in a*b 
  |DIV(exp1, exp2) -> let a = eval_num exp1
                      in let b = eval_num exp2
                      in a/b
  |SIGMA(exp1, exp2, exp3) -> let a = eval_num exp1
                              in let b = eval_num exp2
                              in (if a > b then 0
                                  else sigma exp3 a b)
  |_ -> raise (Failure ("Type error: X is not expected in this function"));;

let rec calculator : exp -> int
= fun exp -> eval_num exp;;
