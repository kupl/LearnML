(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(***********************************)
(**            Problem 1          **)
(***********************************)

module Problem1 = struct
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  

  let rec weightcheck : mobile -> int
  =fun (lb, rb) -> match lb, rb with
				   | SimpleBranch (al, aw), SimpleBranch (bl, bw) -> aw + bw 												  				  	
			       | SimpleBranch (al, aw), CompoundBranch (bl, (mob1, mob2)) -> aw + weightcheck(mob1, mob2)
			       | CompoundBranch (al, (mob1, mob2)), SimpleBranch (bl, bw) -> weightcheck(mob1, mob2) + bw
			       | CompoundBranch (al, (moba1, moba2)), CompoundBranch(bl, (mobb1, mobb2)) -> weightcheck(moba1, moba2) + weightcheck(mobb1, mobb2)

  let rec balanced : mobile -> bool
  =fun (lb, rb) -> match lb, rb with
				   | SimpleBranch (al, aw), SimpleBranch (bl, bw) -> if al*aw = bl*bw then true
				    											  	 else false
				   | SimpleBranch (al, aw), CompoundBranch (bl, (mob1, mob2)) -> if balanced((mob1, mob2))&&(al*aw = bl*weightcheck(mob1, mob2)) then true
				 															  	 else false
				   | CompoundBranch (al, (mob1, mob2)), SimpleBranch (bl, bw) -> if balanced(mob1, mob2)&& (al*weightcheck(mob1, mob2) = bl*bw) then true
				 															  	 else false
				   | CompoundBranch (al, (moba1, moba2)), CompoundBranch (bl, (mobb1, mobb2)) -> if balanced(moba1, moba2)&&balanced(mobb1, mobb2)&& (al*weightcheck(moba1, moba2) = bl*weightcheck(mobb1, mobb2)) then true
				  																			     else false
end






(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let test21 = P ("a", V "a")
  let test22 = P ("a", P ("a", V "a"))
  let test23 = P ("a", P ("b", C (V "a", V "b")))
  let test24 = P ("a", C (V "a", P ("b", V "a")))

  let test2ill1 = P ("a", V "b")
  let test2ill2 = P ("a", C (V "a", P ("b", V "c")))
  let test2ill3 = P ("a", P ("b", C (V "a", V "c")))

  let rec extract : exp -> var list
  =fun e -> match e with 
  			| V v -> []
  		    | P (v1, e1) -> [v1] @ extract e1
  		    | C (e1, e2) -> extract e1 @ extract e2

  let rec search : var * var list -> bool
  =fun (v, lst) -> match lst with
  				   | [] -> false
  				   | head :: [] -> if v = head then true 
  								   else false
  				   | head :: tail -> if ((v = head) || search (v, tail)) then true 
  									 else false
 
  let rec check2 : exp * var list -> bool
  =fun (e, a) -> match e with
  			     | V v1 -> search (v1, a)
  			     | P (v1, e1) -> check2 (e1, a)
  		   	     | C (e1, e2) -> check2 (e1, a) && check2 (e2, a)

  let check : exp -> bool
  =fun e -> check2 (e, extract e)
  
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

  let pgm1 = LETREC ("double", "x", IF (ISZERO (VAR "x"), CONST 0, ADD (CALL (VAR "double", SUB (VAR "x", CONST 1)), CONST 2)), CALL (VAR "double", CONST 6))
  
  let empty_env = fun _ -> raise (Failure "Environment is empty")
  let extend_env (x,v) e = fun y -> if x = y then v else (e y)
  let apply_env e x = e x
  
  let rec eval_bop : (int -> int -> int) -> exp -> exp -> env -> value
  =fun op e1 e2 env ->
  		let v1 = eval e1 env in
  		let v2 = eval e2 env in
  			(match v1, v2 with
  				| Int n1, Int n2 -> Int (op n1 n2)
  				| _ -> raise (Failure "Type Error: non-numeric values for +"))


  and eval : exp -> env -> value
  =fun exp env -> match exp with
  				  | CONST c -> Int c
  				  | VAR x -> apply_env env x 
  				  | ADD (e1, e2) -> eval_bop (+) e1 e2 env
  				  | SUB (e1, e2) -> eval_bop (-) e1 e2 env
  				  | ISZERO e -> (match eval e env with
  				  					| Int n -> if n = 0 then Bool true
  				  							   else Bool false
  				  					| _ -> raise (Failure "Type Error: subexpression of zero? must be Int type"))
  				  | IF (e1, e2, e3) -> (match eval e1 env with
  				  							| Bool true -> eval e2 env
  				  							| Bool false -> eval e3 env
  				  							| _ -> raise (Failure "Type Error: condirion must be Bool type"))
  				  | LET (x, e1, e2) -> let v1 = eval e1 env in
  				  							eval e2 (extend_env (x, v1) env)
  				  | LETREC (f, x, e1, e2) -> eval e2 (extend_env (f, RecProcedure (f, x, e1, env)) env) 
  				  | PROC (x, e) -> Procedure (x, e, env)
  				  | CALL (e1, e2) -> (match eval e1 env with
  				  					  | Procedure (x', e', env') -> eval e' (extend_env (x', (eval e2 env)) env')
  				  					  | RecProcedure (f', x', e', env') -> eval e' (extend_env (x', (eval e2 env)) (extend_env (f', RecProcedure (f', x', e', env)) env'))
  				  					  | _ -> raise (Failure "Type Error: must be Procedure type"))
  				  | _ -> raise (Failure "Type Error: must be expression type")


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

  let pgm2 = LET ("x", CONST 37, PROC ("y", LET ("z", SUB (VAR "y", VAR "x"), SUB (VAR "x", VAR "y"))))
  let pgm3 = LET ("x", CONST 1, LET ("y", CONST 2, ADD (VAR "x", VAR "y")))

  let rec env : exp -> var list
  =fun e -> match e with
  			| LET (v1, c, e1) -> env e1 @ [v1]
  			| PROC (v1, e1) -> env e1
  			| _ -> []

  let rec cntScope : var * int * var list -> int
  =fun (v, n, lst) -> match lst with
  				      | [] -> raise (Failure "Empty")
  				      | head :: tail -> if head = v then n
  				      					else cntScope (v, n + 1, tail)  


  let rec trans : program * var list -> nl_program
  =fun (pgm1, env2) -> match pgm1 with
  			  | CONST c -> NL_CONST c
  			  | VAR v -> NL_VAR (cntScope (v, 0, env2))
  			  | ADD (e1, e2) -> NL_ADD (trans (e1,env2), trans (e2,env2))
  			  | SUB (e1, e2) -> NL_SUB (trans (e1, env2), trans (e2, env2))
  			  | ISZERO e -> NL_ISZERO (trans (e, env2))
  			  | IF (e1, e2, e3) -> NL_IF (trans (e1, env2), trans (e2, env2), trans (e3, env2))
  			  | LET (v, e1, e2) -> NL_LET (trans (e1, env2), trans (e2, v::env2))
  			  | PROC (v, e) -> NL_PROC (trans (e, v::env2))
  			  | CALL (e1, e2) -> NL_CALL (trans (e1, env2), trans (e2, env2))

  let translate : program -> nl_program
  =fun pgm -> trans (pgm, env pgm)

end








(***********************************)
(**            Problem 5          **)
(***********************************)

(*type nl_program = nl_exp
  and nl_exp = 
    | NL_CONST of int
    | NL_VAR of int
    | NL_ADD of nl_exp * nl_exp
    | NL_SUB of nl_exp * nl_exp
    | NL_ISZERO of nl_exp
    | NL_IF of nl_exp * nl_exp * nl_exp
    | NL_LET of nl_exp * nl_exp
    | NL_PROC of nl_exp 
    | NL_CALL of nl_exp * nl_exp*)
module Problem5 = struct
  open Problem4
  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list
  
  let pgm4 = LET ("x", CONST 1, VAR "x")

  let rec nlist: nl_env * int-> nl_value=
  	fun (lst, n) -> match lst with 
  					| [] -> raise (Failure "xxx")
  					| head :: tail -> if n = 0 then head
  									  else nlist (tail, n-1)

  let rec nl_eval : nl_program -> nl_env -> nl_value
  =fun exp env -> match exp with
  				  | NL_CONST c -> NL_Int c
   				  | NL_VAR v -> nlist (env, v)
   				  | NL_ADD (e1, e2) -> let v1 = nl_eval e1 env in
   				  						let v2 = nl_eval e2 env in
   				  						(match v1, v2 with
   				  						 |NL_Int n1, NL_Int n2 -> NL_Int (n1 + n2)
   				  						 | _ -> raise (Failure "xxx"))
  				  | NL_SUB (e1, e2) ->  let v1 = nl_eval e1 env in
   				  						let v2 = nl_eval e2 env in
   				  						(match v1, v2 with
   				  						 |NL_Int n1, NL_Int n2 -> NL_Int (n1 - n2)
   				  						 | _ -> raise (Failure "xxx"))
  				  | NL_ISZERO e -> (match nl_eval e env with
  				  					| NL_Int n -> if n = 0 then NL_Bool true
  				  								  else NL_Bool false
  				  					| _ -> raise (Failure "Type Error: subexpression of zero? must be NL_Int type"))
   				  | NL_IF (e1, e2, e3) -> (match nl_eval e1 env with
  				  							| NL_Bool true -> nl_eval e2 env
  				  							| NL_Bool false -> nl_eval e3 env
  				  							| _ -> raise (Failure "Type Error: condirion must be Bool type"))
    			  | NL_LET (e1, e2) -> let v1 = nl_eval e1 env in
    			  						nl_eval e2 ([v1] @ env)
   				  | NL_PROC e -> NL_Procedure (e, env)
   				  | NL_CALL (e1, e2) -> let proc = nl_eval e1 env in
   				  						let v = nl_eval e2 env in (match proc with
   				  							|NL_Procedure(e,env2) -> nl_eval e env2
   				  							|_->raise (Failure " NO!!!!"))
  				  | _ -> raise (Failure "Type Error: must be expression type")



 let nl_run : nl_program -> nl_value
  =fun pgm -> nl_eval pgm []







end