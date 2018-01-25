
(*u can modify the given function specifications as recursive. *) 
(* 2. However, do not modify the function names or types. *) 
(* 3. It is free to define any helper functions. *) 
(***********************************) (** Problem 1 **) (***********************************) 
module Problem1 = struct type mobile 
= branch * branch 
and branch = SimpleBranch of length * weight 
					| CompoundBranch of length * mobile 
and length = int and weight = int;; 
type env = mobile -> value;;
type value = Int of int | Bool of bool;;

let balanced : mobile -> bool =fun (lb,rb) -> false 

 end 

let rec callb lb env =
match lb with  match lb with 
|SimpleBranch (e1,e2) -> let v1 = callb e1 env in
                           let v2 = callb e2 env in
(match e1,e2 with
 |Int n1, Int n2 -> Int (n1*n2)
 |_-> raise (Failure "ERROR")
 )
 |CompoundBranch (e1, m) -> let v1 = callb e1 env in
  match m with



(***********************************) (** Problem 2 **) (***********************************) 
module Problem2 = struct type exp = V of var | P of var * exp | C of exp * exp and var = string let check : exp -> bool =fun e -> 
 end 
(***********************************) (** Problem 3 **) (***********************************) 
module Problem3 = struct type program = exp and exp = | CONST of int | VAR of var | ADD of exp * exp | SUB of exp * exp | ISZERO of exp | IF of exp * exp * exp | LET of var * exp * exp | LETREC of var * var * exp * exp | PROC of var * exp | CALL of exp * exp and var = string type value = Int of int | Bool of bool | Procedure of var * exp * env | RecProcedure of var * var * exp * env and env = var -> value let empty_env = fun _ -> raise (Failure "Environment is empty") let extend_env (x,v) e = fun y -> if x = y then v else (e y) let apply_env e x = e x let run : program -> value =fun pgm ->
 match pgm with 
|VAR x -> apply_env env x
  |ADD (e1,e2) ->  let v1 = run e1 env in
                  let v2 = run e2 env in
  (match v1,v2 with
 |Int n1, Int n2 -> Int (n1 + n2)
 |_-> raise (Failure "ERROR")
 )
 |SUB (e1,e2)-> let v1 = run e1 env in
               let v2 = run e2 env in
 (match v1,v2 with
 |Int n1, Int n2 -> Int (n1 - n2)
 |_-> raise (Failure "ERROR")
 )
 |ISZERO e -> (match run e env with
 |Int n when n = 0 -> Bool true
 |_-> Bool false
 )
 |if (e1,e2,e3) ->
 (match run e1 env with
 |Bool true -> run e2 env
 |Bool false -> run e3 env
 |_-> "ERROR"
 )
 |LET (x,e1,e2) -> let v1= run e1 env in
 run e2 (extend_env (x,v1) env)
 |LETREC (f,x,e1,e2) -> let f2 = run (f ,x,e1,e2) in
 (match f2 with
 |RecProcedure (f3,x2,e3,e4) -> let v2 = run e2 env in
 dd
 |_->raise (Failure "ERROR")
 end
  
                      
(***********************************) (** Problem 4 **) (***********************************) 
module Problem4 = struct type program = exp and exp = | CONST of int | VAR of var | ADD of exp * exp | SUB of exp * exp | ISZERO of exp | IF of exp * exp * exp | LET of var * exp * exp | PROC of var * exp | CALL of exp * exp and var = string type nl_program = nl_exp and nl_exp = | NL_CONST of int | NL_VAR of int | NL_ADD of nl_exp * nl_exp | NL_SUB of nl_exp * nl_exp | NL_ISZERO of nl_exp | NL_IF of nl_exp * nl_exp * nl_exp | NL_LET of nl_exp * nl_exp | NL_PROC of nl_exp | NL_CALL of nl_exp * nl_exp let translate : program -> nl_program =fun pgm -> NL_CONST 0 (* TODO *) end 

(***********************************) (** Problem 5 **) (***********************************) module Problem5 = struct open Problem4 type nl_value = NL_Int of int | NL_Bool of bool | NL_Procedure of nl_exp * nl_env and nl_env = nl_value list let nl_run : nl_program -> nl_value =fun pgm -> NL_Int 0 (* TODO *) end
