(***********************************)
(**            Problem 1          **)
(***********************************)

module Problem1 = struct
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int 

  let rec weight2 mobile =
    let rec weight1 branch = match branch with
      SimpleBranch (l, w) -> w
      |CompoundBranch (l, m) -> weight2 (m) in
      match mobile with
        (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
        |(SimpleBranch (l1, w1), CompoundBranch (l2, m)) -> w1 + weight1 (CompoundBranch (l2, m))
        |(CompoundBranch (l1, m), SimpleBranch (l2, w2)) -> w2 + weight1 (CompoundBranch (l1, m))
        |(CompoundBranch (l1, w1), CompoundBranch(l2, w2)) -> weight1 (CompoundBranch (l1, w1))+weight1 (CompoundBranch (l2, w2))
  
  let rec weight1 branch = match branch with
    SimpleBranch (l, w) -> w
    |CompoundBranch (l, m) -> weight2 (m)

  let rec balanced_once : mobile -> bool
  =fun (lb,rb) -> match lb with 
    SimpleBranch (l1, w1) -> (match rb with
                              SimpleBranch (l2, w2)->if w1*l1 = w2*l2 then true else false
                              |CompoundBranch (l2, m)->if w1*l1 = weight2(m)*l2 then true else false)
    |CompoundBranch (l1, m1) -> (match rb with
                              SimpleBranch (l2, w)->if l2*w = l1*weight2(m1) then true else false
                              |CompoundBranch (l2, m2)->if l2*weight2 (m2)=l1*weight2(m1) then true else false)

  let rec balanced : mobile -> bool
  =fun (lb, rb) -> match lb with
    SimpleBranch (l1, w1) -> (match rb with
                              SimpleBranch (l2, w2)-> balanced_once (lb, rb)
                              |CompoundBranch (l2, m)->if ((balanced_once (lb, rb)=true)&&(balanced_once (m)=true)=true) then true else false)
    |CompoundBranch (l1, m1) -> (match rb with
                              SimpleBranch (l2, w)->if ((balanced_once (lb, rb)=true)&&(balanced_once (m1)=true)=true) then true else false
                              |CompoundBranch (l2, m2)->if ((balanced_once (lb, rb)=true)&&(balanced_once (m1)=true)&&(balanced_once (m2)=true)) then true else false)
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec compare v l= match l with
    [] -> false
    |hd::tl -> if hd=v then true else compare v tl

  let rec makel e l = match e with
     V(v)->compare v l
    |P(v, exp1)->makel exp1 (l@[v])
    |C(exp2, exp3)->makel exp2 l&&makel exp3 l

  let check : exp -> bool
   =fun e ->makel e []

end

(***********************************)
(**            Problem 3          **)
(***********************************)
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
  
  let empty_env = fun _ -> raise (Failure "Environment is empty")
  let extend_env (x,v) e = fun y -> if x = y then v else (e y)
  let apply_env e x = e x 

   let rec eval : exp -> env -> value
=fun exp env -> 
match exp with 
| CONST n -> Int n 
| VAR x -> apply_env env x 
| ADD (e1,e2) -> let v1 = eval e1 env in let v2 = eval e2 env in 
(match v1,v2 with 
| Int n1, Int n2 -> Int (n1 + n2) 
| _ -> raise (Failure "Type Error: non-numeric values")) 
| SUB (e1,e2) -> 
let v1 = eval e1 env in let v2 = eval e2 env in (match v1,v2 with 
| Int n1, Int n2 -> Int (n1 - n2) 
| _ -> raise (Failure "Type Error: non-numeric values"))
| ISZERO e -> (match eval e env with | Int n when n = 0 -> Bool true | _ -> Bool false) 
| IF (e1,e2,e3) -> 
(match eval e1 env with 
| Bool true -> eval e2 env 
| Bool false -> eval e3 env 
| _ -> raise (Failure "Type Error: condition must be Bool type")) 
| LET (x,e1,e2) -> let v1 = eval e1 env in eval e2 (extend_env (x,v1) env)
| LETREC (y, x, e1, e2) -> eval e2 (extend_env (y, RecProcedure(y, x, e1, env)) env)
| PROC (x, e) -> Procedure (x, e, env)
| CALL (e1,e2)->match eval e1 env with
                            | Procedure (x,e,env2)->(match eval e2 env with 
                                                    |v2-> eval e (extend_env (x,v2) env2))
                            | RecProcedure (y,x,e,env2)->(match eval e2 env with 
                                                          |v2->eval e (extend_env (x,v2) (extend_env (y,RecProcedure (y,x,e,env2)) env2)))  
                            |_->raise (Failure "Type Error: Procedure error")

let run : program -> value
  =fun pgm -> eval pgm empty_env

end
(***********************************)
(**            Problem 4          **)
(***********************************)

module Problem4 = struct
  type program = exp
  and exp = 
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
  and var = string
  
  type nl_program = nl_exp
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

  let rec compare v l idx = match l with
    [] -> raise (Failure "ERROR")
    |hd::tl -> if(hd=v) then idx else compare v tl idx+1

  let rec translate2 : exp -> string list -> nl_exp 
  =fun e l -> match e with
  CONST n -> NL_CONST n
  |VAR v -> NL_VAR (compare v l 0)
  |ADD (e1, e2) -> NL_ADD (translate2 e1 l, translate2 e2 l)
  |SUB (e1, e2) -> NL_SUB (translate2 e1 l, translate2 e2 l)
  |ISZERO e -> NL_ISZERO (translate2 e l)
  |IF (e1, e2, e3) -> NL_IF (translate2 e1 l, translate2 e2 l, translate2 e3 l)
  |LET (x, e2, e3) -> let p = x::l in
                              NL_LET(translate2 e2 l, translate2 e3 p)
  |PROC (x, e) -> let p = x::l in
                  NL_PROC (translate2 e p)
  |CALL (e1, e2) -> NL_CALL (translate2 e1 l, translate2 e2 l)

  let translate
  =fun pgm ->translate2 pgm []

end

(***********************************)
(**            Problem 5          **)
(***********************************)

module Problem5 = struct
  open Problem4
  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> NL_Int 0 (* TODO *)
end