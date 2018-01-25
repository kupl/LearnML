

(** Problem 1 **) 
(*¤·¤»*) 
type mobile = branch * branch 
and branch = SimpleBranch of length * weight 
			|CompoundBranch of length * mobile 
and length = int 
and weight = int 
let balanced : mobile -> bool =fun (lb,rb) -> false 

let rec findweight b 
 = match b with
|SimpleBranch(l,w) ->  w 
|CompoundBranch ( l , (f1,f2) ) -> (findweight f1 +findweight f2) 

 
let rec findvalue b 
= match b with
|SimpleBranch ( l , w ) -> l * w  
|CompoundBranch ( l , m ) -> l * ( findweight b )
 
let rec isbalanced m= match m with 
|(b1, b2) -> if ( (findvalue b1) = (findvalue b2) ) then true else false


let balanced m= match m with 
|(SimpleBranch (l1,w1), SimpleBranch(l2,w2) )  -> (isbalanced m) 
|(SimpleBranch (l1,w1), CompoundBranch(l2,m2) ) -> (isbalanced m) && (isbalanced m2)
|(CompoundBranch (l1,m1), SimpleBranch(l2,w2) ) -> (isbalanced m) && (isbalanced m1)
|(CompoundBranch (l1,m1), CompoundBranch(l2,m2)) -> (isbalanced m) && (isbalanced m1) && (isbalanced m2)

   


(** Problem 2 **) 
(***********************************) 

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
 
   let check : exp -> bool
  =fun e -> true
 
let rec exist v lst = match lst with 
| [] -> false
| hd::tl ->  if (v = hd) then true else ( exist v tl) 
 
let rec finding e lst = match e with
| V (v) ->  exist v lst 
| P (v,e) ->  finding e (lst@[v])
| C (e1,e2) -> if ((finding e1 lst) && (finding e2 lst)) then true else false
 
let rec check : exp -> bool
= fun e -> finding e []
 
end


(** Problem 3 **) 
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
| LET (x,e1,e2) -> let v1 = eval e1 env in eval e2 (extend_env (x,v1) env)
		
| PROC (var, exp) -> Procedure (var, exp, env)
| LETREC (var1, var2, exp1, exp2)-> 
		eval exp2 (extend_env (var1, RecProcedure(var1,var2,exp1,env))env)
	
| CALL (e1,e2)->(match eval e1 env with | Procedure (x,e,env2)->(match eval e2 env with |v2-> eval e (extend_env (x,v2) env2))
					| RecProcedure (f,x,e,env2)->(match eval e2 env with |v2->eval e (extend_env (x,v2) (extend_env (f,RecProcedure (f,x,e,env2)) env2)))  
				       |_->raise (Failure "Type Error: condition must be Procedure") )

  let run : program -> value
  =fun pgm -> eval pgm (fun _ -> raise(Failure "error"))
  

end

 
(** Problem 4 **) 
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

let translate : program -> nl_program =fun pgm -> NL_CONST 0
 
let rec number: var -> var list-> int -> int 
=fun v lst a-> match lst with
	|[]-> raise(Failure"error")
	| hd::tl-> if (v=hd) then a else (number v tl a+1) 

let rec bft pgm a =  match pgm with
	| CONST i-> NL_CONST i
    | VAR v -> NL_VAR (number v a 0)
    | ADD (e1,e2) -> NL_ADD(bft e1 a, bft e2 a) 
    | SUB (e1,e2) -> NL_SUB( (bft e1 a) , (bft e2 a) )
    | ISZERO e-> NL_ISZERO (bft e a)
    | IF (e1,e2,e3) -> NL_IF((bft e1 a),(bft e2  a),(bft e3  a))
    | LET (var,exp1,exp2) -> NL_LET (bft exp1 a  ,bft exp2 ([var]@a) )
    | PROC (var,exp) -> NL_PROC( bft exp ([var]@a) )
    | CALL (e1,e2) -> NL_CALL( (bft e1 a), (bft e2 a) )

 let translate : program -> nl_program
  =fun pgm -> bft pgm []
 
  end


(***********************************)
(**            Problem 5          **)
(***********************************)

module Problem5 = struct
  open Problem4

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


 
  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list


let translate : program -> nl_program =fun pgm -> NL_CONST 0
 
let rec number: var -> var list-> int -> int 
=fun v lst a-> match lst with
	|[]-> raise(Failure"error")
	| hd::tl-> if (v=hd) then a else (number v tl a+1) 

let rec bft pgm a =  match pgm with
	| CONST i-> NL_CONST i
    | VAR v -> NL_VAR (number v a 0)
    | ADD (e1,e2) -> NL_ADD(bft e1 a, bft e2 a) 
    | SUB (e1,e2) -> NL_SUB( (bft e1 a) , (bft e2 a) )
    | ISZERO e-> NL_ISZERO (bft e a)
    | IF (e1,e2,e3) -> NL_IF((bft e1 a),(bft e2  a),(bft e3  a))
    | LET (var,exp1,exp2) -> NL_LET (bft exp1 a  ,bft exp2 ([var]@a) )
    | PROC (var,exp) -> NL_PROC( bft exp ([var]@a) )
    | CALL (e1,e2) -> NL_CALL( (bft e1 a), (bft e2 a) )

 let translate : program -> nl_program
  =fun pgm -> bft pgm []

let empty_env = fun _ -> raise (Failure "Environment is empty")
let extend_env a env = a::env
let apply_env v env : int -> nl_env -> int= fun v env ->0
let apply_env v env= match env with 
				|[]-> raise(Failure"error")
				|hd::tl-> if (v=0) then hd else (apply_env (v-1) tl ) 
(*	
let realvalue prg=  match prg with
				|NL_Int i ->i  
                |_-> raise( Failure "Error")
  *)
	
let rec bfr : nl_exp -> nl_env -> nl_value=fun prg env
-> match prg with  
  
  | NL_CONST i->  NL_Int i 
 
  | NL_VAR v ->  List.nth env v
 
  | NL_ADD (e1,e2) -> let v1=(bfr e1 env ) in let v2=(bfr e2 env) in (match v1,v2 with		
			|NL_Int n1, NL_Int n2->NL_Int (n1+n2)
			|_-> raise(Failure"error"))
 | NL_SUB (e1,e2) -> let v1=(bfr e1 env ) in let v2=(bfr e2 env) in (match v1,v2 with		
			|NL_Int n1, NL_Int n2->NL_Int (n1-n2)
			|_-> raise(Failure"error"))
			
  | NL_ISZERO e-> (match e with 
			|NL_CONST 0-> NL_Bool true
			|_ -> NL_Bool false
				)
  
  | NL_IF (e1,e2,e3) ->(match (bfr e1 env) with 
			| NL_Bool true -> (bfr e2 env)
			| NL_Bool false -> (bfr e3 env)
			|_-> raise (Failure "error")
						)
			
  |NL_LET (e1,e2)-> let a = bfr e1 env in (bfr e2 (extend_env a env))
  
  | NL_PROC (e1) -> NL_Procedure (e1, env)

	

  | NL_CALL (e1,e2) -> (match bfr e1 env with 
						| NL_Procedure (e3,env2)->(match bfr e2 env with 
									|v2-> bfr e3 (extend_env v2 env2))
									|_->raise (Failure "error") )
              
 
let nl_run : nl_program -> nl_value
   =fun pgm -> bfr pgm []  

 end