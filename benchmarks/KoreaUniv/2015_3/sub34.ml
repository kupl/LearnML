(* 1. You can modify the given function specifications as recursive. *) (* 2. However, do not modify the function names or types. *) (* 3. It is free to define any helper functions. *) (***********************************) (** Problem 1 **) (***********************************) module Problem1 = struct type mobile = branch * branch and branch = SimpleBranch of length * weight | CompoundBranch of length * mobile and length = int and weight = int let balanced : mobile -> bool =fun (lb,rb) -> false 

let rec getWeight b =
match b with
|SimpleBranch(le, we) ->  we 
|CompoundBranch (l,(lb,rb)) -> (getWeight lb + getWeight rb) 
 
let rec getMultiple b = 
match b with
|SimpleBranch (le, we) -> le * we  
|CompoundBranch (le, m) -> le * (getWeight b)
 
let rec compare m = 
match m with 
|(lb, rb)-> 
   if((getMultiple lb) = (getMultiple rb)) then true 
   else false
  
let balanced m =
match m with 
|(SimpleBranch (le1,we1), SimpleBranch(le2,we2))  -> compare m 
|(SimpleBranch (le1,we1), CompoundBranch(le2,m2)) -> (compare m) && (compare m2)
|(CompoundBranch (le1,m1), SimpleBranch(le2,we2) ) -> (compare m) && (compare m1)
|(CompoundBranch (le1,m1), CompoundBranch(le2,m2)) -> (compare m) && (compare m1) && (compare m2) 
end 


(***********************************) (** Problem 2 **) (***********************************) module Problem2 = struct type exp = V of var | P of var * exp | C of exp * exp and var = string let check : exp -> bool =fun e -> true 

let var v = V(v)
  let proc v e = P(v, e)
  let call e1 e2 = C(e1, e2)
  
  let rec free_vars = function
   V(v) -> [v]
   | P(v, e) -> List.filter (fun x -> x<>v) (free_vars e)
   | C(e1, e2) ->
      let f_e1 = free_vars e1 in 
      let f_e2 = free_vars e2 in
        List.append f_e1 (List.filter (fun x -> not (List.mem x f_e1)) f_e2)

  let rec fresh_var v1 l =
   if List.mem v1 l then fresh_var(v1 ^ "'") l
   else v1

  let check : exp -> bool
  = fun e ->
  if free_vars e = [] then true
  else false


end 


(***********************************) (** Problem 3 **) (***********************************) module Problem3 = struct type program = exp and exp = | CONST of int | VAR of var | ADD of exp * exp | SUB of exp * exp | ISZERO of exp | IF of exp * exp * exp | LET of var * exp * exp | LETREC of var * var * exp * exp | PROC of var * exp | CALL of exp * exp and var = string type value = Int of int | Bool of bool | Procedure of var * exp * env | RecProcedure of var * var * exp * env and env = var -> value let empty_env = fun _ -> raise (Failure "Environment is empty") let extend_env (x,v) e = fun y -> if x = y then v else (e y) let apply_env e x = e x let run : program -> value =fun pgm -> Int 0 


let rec eval : exp -> env -> value
=fun exp env -> 
match exp with 
| CONST n -> Int n 
| VAR x -> applyEnv env x 
| ADD (e1,e2) -> let v1 = eval e1 env in let v2 = eval e2 env in (match v1,v2 with 
                           | Int n1, Int n2 -> Int (n1 + n2) 
                           | _ -> raise (Failure "Type Error: non-numeric values")) 
| SUB (e1,e2) -> let v1 = eval e1 env in let v2 = eval e2 env in (match v1,v2 with 
                           | Int n1, Int n2 -> Int (n1 - n2) 
                           | _ -> raise (Failure "Type Error: non-numeric values"))
| ISZERO e -> (match eval e env with | Int n when n = 0 -> Bool true | _ -> Bool false) 
| IF (e1,e2,e3) -> (match eval e1 env with 
            | Bool true -> eval e2 env 
            | Bool false -> eval e3 env 
            | _ -> raise (Failure "Type Error: condition must be Bool type")) 
| LET (x,e1,e2) -> let v1 = eval e1 env in eval e2 (extendEnv (x,v1) env)
| PROC (x,e1)-> Procedure (x,e1,env)
| CALL (e1,e2)->
(match eval e1 env with 
| Procedure (x,e,env2)->
   (match eval e2 env with 
   |v2-> eval e (extendEnv (x,v2) env2))
| RecProcedure (f,x,e,env2)->
   (match eval e2 env with 
   |v2->eval e (extendEnv (x,v2) (extendEnv (f,RecProcedure (f,x,e,env2)) env2)))  
        |_->raise (Failure "Type Error: condition must be Procedure") )
| LETREC (f,x,e1,e2) -> eval e2 (extendEnv (f,RecProcedure(f,x,e1,env)) env)

  let run : program -> value
  =fun pgm -> eval pgm emptyEnv
 

 end 



(***********************************) (** Problem 4 **) (***********************************) module Problem4 = struct type program = exp and exp = | CONST of int | VAR of var | ADD of exp * exp | SUB of exp * exp | ISZERO of exp | IF of exp * exp * exp | LET of var * exp * exp | PROC of var * exp | CALL of exp * exp and var = string type nl_program = nl_exp and nl_exp = | NL_CONST of int | NL_VAR of int | NL_ADD of nl_exp * nl_exp | NL_SUB of nl_exp * nl_exp | NL_ISZERO of nl_exp | NL_IF of nl_exp * nl_exp * nl_exp | NL_LET of nl_exp * nl_exp | NL_PROC of nl_exp | NL_CALL of nl_exp * nl_exp let translate : program -> nl_program =fun pgm -> NL_CONST 0 (* TODO *) end (***********************************) (** Problem 5 **) (***********************************) module Problem5 = struct open Problem4 type nl_value = NL_Int of int | NL_Bool of bool | NL_Procedure of nl_exp * nl_env and nl_env = nl_value list let nl_run : nl_program -> nl_value =fun pgm -> NL_Int 0 (* TODO *) end 