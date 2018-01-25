module Problem1 = struct


type mobile = branch * branch
and branch = SimpleBranch of length * weight
			| CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced mobile =
	match mobile with
		(SimpleBranch(simLen1,simWei1),SimpleBranch(simLen2,simWei2)) -> if (simLen1*simWei1) = (simLen2*simWei2) then true else false
		|(SimpleBranch(simLen1,simWei1),CompoundBranch(compLen1,compMob1)) -> if balanced compMob1 = false then false else if 
																				(simLen1*simWei1) = mobWeightR(compMob1,compLen1) then true
																				else false
		|(CompoundBranch(compLen1,compMob1),SimpleBranch(simLen1,simWei1)) -> if balanced compMob1 = false then false else if
																				mobWeightL(compMob1,compLen1) = simLen1*simWei1 then true else false
		|(CompoundBranch(compLen1,compMob1),CompoundBranch(compLen2,compMob2)) -> if balanced compMob1 = false then false 
																				else if balanced compMob2 = false then false else
																				if mobWeightL(compMob1,compLen1)=mobWeightR(compMob2,compLen2) then true
																				else false
and mobWeightL =
	fun (mobile,length)->  
	match mobile with
		(SimpleBranch(simLen1,simWei1),SimpleBranch(simLen2,simWei2))-> (length+simLen1)*simWei1 + (length-simLen2)*simWei2
		|(SimpleBranch(simLen1,simWei1),CompoundBranch(compLen1,compMob1)) -> (length+simLen1)*simWei1 + mobWeightR(compMob1,length - compLen1)
		|(CompoundBranch(compLen1,compMob1),SimpleBranch(simLen1,simWei1)) -> mobWeightL (compMob1,length+compLen1) + (length-simLen1)*simWei1
		|(CompoundBranch(compLen1,compMob1),CompoundBranch(compLen2,compMob2))-> mobWeightL(compMob1,length+compLen1) + 
																		mobWeightR(compMob2,length-compLen2)
and mobWeightR = 
	fun (mobile,length)->
	match mobile with
		(SimpleBranch(simLen1,simWei1),SimpleBranch(simLen2,simWei2))->(length-simLen1)*simWei1 + (length+simLen2)*simWei2
		|(SimpleBranch(simLen1,simWei1),CompoundBranch(compLen1,compMob1)) -> (length-simLen1)*simWei1 + mobWeightR (compMob1,length + compLen1)
		|(CompoundBranch(compLen1,compMob1),SimpleBranch(simLen1,simWei1)) -> mobWeightL (compMob1,length-compLen1) + (length+simLen1)*simWei1
		|(CompoundBranch(compLen1,compMob1),CompoundBranch(compLen2,compMob2))-> mobWeightL(compMob1,length-compLen1) +
																		mobWeightR(compMob2,length+compLen2)
end

module Problem2 = struct
	
type exp = V of var
		| P of var * exp  (*procedure*)
		| C of exp * exp  (*call expression(procedure)*)
and var = string

let rec check exp =
			match exp with
				V var -> false
				|_->checkExp(exp,[])


and checkExp = fun(exp,env)->
		match exp with
			V var-> (match env with 	
				[]-> false
				|hd::tail-> if hd = var then true else checkExp (exp,tail)
				)
			|P (var,exp)-> 	checkExp(exp,(var::env))
			
			|C (exp1,exp2)->  
				if checkExp(exp1,env) = true then 
				if checkExp(exp2,env) = true then true else false else false
end


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


let rec eval_bop = fun op exp1 exp2 env ->
			let value1 = eval exp1 env in
			let value2 = eval exp2 env in
				(match value1,value2 with
				 |Int int1, Int int2 -> Int (op int1 int2)
				 |_->raise (Failure "NOT_MATCH")
				 )


and eval = fun exp env ->
		 match exp with
			|CONST n -> Int n
			|VAR x -> apply_env env x
			|ADD (exp1,exp2) -> eval_bop (+) exp1 exp2 env
			|SUB (exp1,exp2) -> eval_bop (-) exp1 exp2 env
			|ISZERO exp -> (let value = eval exp env in
								match value with
								|Int n -> if n=0 then Bool true else Bool false
								|_->raise(Failure "TYPE_ERROR")
					)
			|IF (exp1,exp2,exp3)->
								(match eval exp1 env with
								 |Bool true -> eval exp2 env
								 |Bool false-> eval exp3 env
								 |_->raise (Failure "TYPE_ERROR")
								 )
			|LET (var,exp1,exp2)->
								let value1 = eval exp1 env in
									eval exp2 (extend_env (var,value1) env )
			|LETREC (var1,var2,exp1,exp2)->
								eval exp2 (extend_env (var1, RecProcedure(var1,var2,exp1,env) ) env )
			|PROC (var,exp)-> Procedure (var,exp,env)	
			|CALL (exp1,exp2)-> (match eval exp1 env with
								|RecProcedure(var1,var2,pexp1,envprime)->
									let v = eval exp2 env in
										eval pexp1( extend_env (var2,v) (extend_env (var1,RecProcedure(var1,var2,pexp1,envprime)) envprime)  )
								
								|Procedure(var,pexp1,envprime)->	
									let v = eval exp2 env in
										eval pexp1(extend_env (var,v) env)
								|_->raise (Failure "TYPE_ERROR")

								)

let run : program -> value
=fun pgm -> eval pgm empty_env
end

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

and env = (var) list

exception NOT_FOUND
let empty =[]
let rec lookup x e index = 
	match e with
	[]-> raise NOT_FOUND
	| (y)::t1->if x=y then index else lookup x t1 (index+1)
let extend x e = (x)::e


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




let rec translating = fun pgm env ->
		match pgm with
		|CONST int1-> NL_CONST int1
		|VAR var -> NL_VAR (lookup var env 0)
		|ADD (exp1,exp2) -> NL_ADD (translating exp1 env,translating exp2 env)
		|SUB (exp1,exp2) -> NL_SUB (translating exp1 env,translating exp2 env)
		|ISZERO exp -> NL_ISZERO (translating exp env)
		|IF (exp1,exp2,exp3)-> NL_IF(translating exp1 env, translating exp2 env, translating exp3 env)
		|CALL (exp1,exp2)-> NL_CALL(translating exp1 env, translating exp2 env)
		|PROC (var,exp1)-> NL_PROC(translating exp1 (extend var env) )
		|LET (var,exp1,exp2) -> NL_LET(translating exp1 env, translating exp2 (extend var env))	

let translate : program -> nl_program
=fun pgm -> translating pgm empty 
end


module Problem5 = struct
	open Problem4

type nl_value = NL_Int of int 
              | NL_Bool of bool 
              | NL_Procedure of nl_exp * nl_env
and nl_env = nl_value list

exception NOT_FOUND
let nl_empty = []
let rec nl_lookup index e = match e with
				[]->raise NOT_FOUND
				|hd::t1-> if index=0 then hd else nl_lookup (index-1) e
let nl_extend v e = v::e

let rec interprete_bop = fun op exp1 exp2 env ->
				let v1 = interprete exp1 env in
				let v2 = interprete exp2 env in
				(match v1,v2 with
				 |NL_Int int1,NL_Int int2 -> NL_Int (op int1 int2)

				 |_->raise (Failure "NOT_MATCH")
				 )


and interprete = fun exp env -> match exp with					
					|NL_CONST int1-> NL_Int int1
					|NL_VAR int1-> nl_lookup int1 env
					|NL_ADD (exp1,exp2)-> interprete_bop (+) exp1 exp2 env
					|NL_SUB (exp1,exp2)-> interprete_bop (-) exp1 exp2 env
					|NL_ISZERO exp ->( let v1 = interprete exp env in
							match v1 with
							|NL_Int n->if n=0 then NL_Bool true else NL_Bool false
							|_->raise(Failure "Type Error")
							)
					|NL_IF (exp1,exp2,exp3)->(match interprete exp1 env with
								|NL_Bool true ->interprete exp2 env
								|NL_Bool false->interprete exp3 env
								|_->raise(Failure "Type_Error")
							)
					|NL_LET (exp1,exp2)-> interprete exp2 (nl_extend (interprete exp1 env) env)
					|NL_PROC exp -> NL_Procedure(exp,env)
									
					|NL_CALL (exp1,exp2) ->(match interprete exp1 env with
											|NL_Procedure (pexp,envprime)->
												let v = interprete exp2 env in
													interprete pexp (nl_extend (v) envprime )
								|_->raise (Failure "Type_Error")	
							)

let nl_run : nl_program -> nl_value
=fun pgm -> interprete pgm nl_empty
end
