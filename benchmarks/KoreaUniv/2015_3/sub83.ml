(* 1. You can modify the given checkern specifications as recursive. *) (* 2. However, do not modify the checkern names or types. *) (* 3. It is free to define any helper checkerns. *) 
(***********************************) (** Problem 1 **) (***********************************) 
module Problem1 = struct
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec branchWeight : branch -> weight 
  =fun (x) ->
  match x with
  |SimpleBranch(l,w) -> w
  |CompoundBranch(l,w) -> let rec mobileWeight : mobile -> int
  =fun (y) ->
  match y with
  |(a,b) -> branchWeight(a) + branchWeight(b) in mobileWeight(w)

  let rec mobileWeight : mobile -> int
  =fun (m) ->
  match m with
  |(a,b) -> branchWeight(a) + branchWeight(b)

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match lb with 
  |SimpleBranch (l1,w1) ->(
      match rb with
      |SimpleBranch (l2,w2) ->
         if (l1*w1=l2*w2) then true else false
      |CompoundBranch (l2,w2) -> 
         if (balanced(w2)) then if (l1*w1=l2*mobileWeight(w2)) then true else false else false
            )
  |CompoundBranch (l1,w1) -> 
      (match rb with
      |SimpleBranch (l2,w2) ->
         if (balanced(w1)) then if (l2*w2=l1*mobileWeight(w1)) then true else false else false
      |CompoundBranch (l2,w2) -> 
           if(balanced(w1)&&balanced(w2))  then if (l2*mobileWeight(w2)=l1*mobileWeight(w1)) then true else false else false
    )
end
(***********************************) (** Problem 2 **) (***********************************) 
module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
let rec exist v lst = match lst with 
| [] -> false
| hd::tl ->  if (v = hd) then true else ( exist v tl) 
 
let rec checker e lst = match e with
| V (v) -> exist v lst 
| P (v,e) -> checker e (lst@[v])
| C(e1,e2) ->if ((checker e1 lst) = (checker e2 lst)) then true else false
 
let rec check : exp -> bool
= fun e -> checker e []
end
(***********************************) (** Problem 3 **) (***********************************) 
module Problem3 = struct
  type program = exp
  and exp = 
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | LETREC of var * var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
  and var = string
 
  type value = Int of int | Bool of bool 
             | Procedure of var * exp * env 
             | RecProcedure of var * var * exp * env
  and env = var -> value
  
  let emptyEnv = fun _ -> raise (Failure "Environment is empty")
  let extendEnv (x,v) e = fun y -> if x = y then v else (e y)
  let applyEnv e x = e x
  

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
| PROC (x,e1)->Procedure (x,e1,env)
| CALL (e1,e2)->(match eval e1 env with | Procedure (x,e,env2)->(match eval e2 env with |v2-> eval e (extendEnv (x,v2) env2))
               | RecProcedure (f,x,e,env2)->(match eval e2 env with |v2->eval e (extendEnv (x,v2) (extendEnv (f,RecProcedure (f,x,e,env2)) env2)))  
                   |_->raise (Failure "Type Error: condition must be Procedure") )
| LETREC (f,x,e1,e2) -> eval e2 (extendEnv (f,RecProcedure(f,x,e1,env)) env)

  let run : program -> value
  =fun pgm -> eval pgm emptyEnv
 
end
(***********************************) (** Problem 4 **) (***********************************) 
module Problem4 = struct type program = exp 
and exp = 
| CONST of int 
| VAR of var 
| ADD of exp * exp 
| SUB of exp * exp 
| ISZERO of exp 
| IF of exp * exp * exp 
| LET of var * exp * exp 
| PROC of var * exp 
| CALL of exp * exp and var = string type nl_program = nl_exp 
and nl_exp = 
| NL_CONST of int 
| NL_VAR of int 
| NL_ADD of nl_exp * nl_exp 
| NL_SUB of nl_exp * nl_exp 
| NL_ISZERO of nl_exp 
| NL_IF of nl_exp * nl_exp * nl_exp 
| NL_LET of nl_exp * nl_exp 
| NL_PROC of nl_exp 
| NL_CALL of nl_exp * nl_exp 

let varList = []

let putVarList varList k = (match varList with
	|[]->[k]
	|hd::tl -> k::hd::tl)
	
let cnt = 0 
let rec countVar varList x cnt= 
		(match varList with
			|[]->0
			|hd::tl -> if hd!=x then countVar tl x cnt+1 else cnt) 

let rec translate : program -> nl_program =fun pgm -> 
	(*let rec translate2 pgm varList cnt = *)
	match pgm with
|CONST x -> NL_CONST x
|VAR x -> NL_CONST (countVar varList x 0)
|ADD(x,y) -> NL_ADD(translate x,translate y)
|SUB(x,y)->NL_SUB(translate x, translate y)
|ISZERO(x)->NL_ISZERO(translate x)
|IF(x,y,z)->NL_IF(translate x,translate y,translate z)
|LET(x,y,z)->NL_LET(translate y,translate z)
|PROC(x,y)->NL_PROC(translate y)
|CALL(x,y)->NL_CALL(translate x, translate y)
(* TODO *) end 
(***********************************) (** Problem 5 **) (***********************************) 
module Problem5 = struct open Problem4 
type nl_value = NL_Int of int 
| NL_Bool of bool 
| NL_Procedure of nl_exp * nl_env 
and nl_env = nl_value list 
let nl_run : nl_program -> nl_value =fun pgm ->
NL_Int 0 (* TODO *) end 