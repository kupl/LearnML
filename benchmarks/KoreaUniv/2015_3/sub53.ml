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
	
  let rec totweight : mobile -> int
	=fun (lb,rb) -> 
	match lb with
	|SimpleBranch(l,w) -> 
		(match rb with
		|SimpleBranch(l2,w2) -> w + w2
		|CompoundBranch(l2,m) -> w + (totweight m))
	|CompoundBranch(l,m) ->
		(match rb with
		|SimpleBranch(l2,w2) -> (totweight m) + w2
		|CompoundBranch(l2,m2) -> (totweight m) + (totweight m2))


  let rec balanced : mobile -> bool
	=fun (lb,rb) -> 
  	match lb with
 	|SimpleBranch(len_l,wei_l) ->
   		(match rb with
    		|SimpleBranch(len_r,wei_r) -> if len_l*wei_l = len_r*wei_r then true else false
    		|CompoundBranch(len_r,m) -> if len_r*(totweight m) = len_l*wei_l then balanced m else false)
  	|CompoundBranch(len_l,m) ->
  		(match rb with
   		 |SimpleBranch(len_r,wei_r) -> if len_l*(totweight m) = len_r*wei_r then balanced m else false
   		 |CompoundBranch(len_r,m2) -> if len_l*(totweight m) = len_r*(totweight m2) then (balanced m)&&(balanced m2) else false)


end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec check_list(exp, lst) = 
	match exp with
	|V(var) -> if List.mem var lst then true else false
	|P(var,exp) -> check_list(exp, lst@[var])
	|C(exp1, exp2) -> check_list(exp2,lst) && check_list(exp2,lst)

  let check : exp -> bool
  =fun e -> check_list(e,[])
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
  
  let empty_env = fun _ -> raise (Failure "empty environment")
  let extend_env (x,v) e = fun y -> if x = y then v else (e y)
  let apply_env e x = e x

  let rec eval : exp -> env -> value
	=fun exp env -> 
	match exp with
	|CONST n -> Int n
 	|VAR x -> apply_env env x
	|ADD (exp1, exp2) ->
		let v1 = eval exp1 env in
		let v2 = eval exp2 env in
			(match v1, v2 with
			|Int n1, Int n2 -> Int(n1 + n2)
			|_ -> raise(Failure "Type Error : non-numeric values"))
	|SUB(exp1, exp2) ->
		let v1 = eval exp1 env in
		let v2 = eval exp2 env in
			(match v1, v2 with
			|Int n1, Int n2 -> Int(n1 - n2)
			|_ -> raise(Failure "Type Error : non-numeric values"))
	|ISZERO exp -> 
		(match eval exp env with
		|Int n when n=0 -> Bool true
		|_ -> Bool false)
	|IF (exp1, exp2, exp3) ->
		(match eval exp1 env with
		|Bool true -> eval exp2 env
		|Bool false -> eval exp3 env
		|_ -> raise(Failure "Type Error : condition must be Bool Type"))
	|LET (x, exp1, exp2) ->
		let v1 = eval exp1 env in 
		eval exp2 (extend_env (x, v1) env) 
	|LETREC (f, x, exp1, exp2) -> eval exp2 (extend_env (f, RecProcedure(f, x, exp1, env)) env)
	|PROC (x, exp) -> Procedure (x, exp, env)
	|CALL (exp1, exp2) -> 
		(match eval exp1 env with
		|Procedure(x, exp, env1) ->
			let v = eval exp2 env in
			eval exp (extend_env(x, v) env1)
		|RecProcedure(f, x, exp, env1) ->
			let v = eval exp2 env in
			eval exp (extend_env (f, RecProcedure(f, x, exp, env1)) ((extend_env(x, v)) env))
		|_ -> raise(Failure "Type Error")
		)
  
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

  let rec c : string list * string * int -> int
	= fun (lst, x, cnt) ->
		match lst with
		|[] -> raise(Failure "error!!")
		|hd::tl -> if hd = x then cnt else c(tl, x, cnt+1)
  
  let rec n : program * string list -> nl_program
	= fun (exp, lst) ->
		match exp with
		|CONST n -> NL_CONST n
		|VAR v -> NL_VAR (c(lst, v, 0))
		|ADD (exp1, exp2) -> NL_ADD(n(exp1, lst), n(exp2, lst))
		|SUB (exp1, exp2) -> NL_SUB(n(exp1, lst), n(exp2, lst))
		|ISZERO (exp) -> NL_ISZERO(n(exp, lst))
		|IF (exp1, exp2, exp3) -> NL_IF(n(exp1, lst), n(exp2, lst), n(exp3, lst))
		|LET(x, exp1, exp2) -> NL_LET(n(exp1, lst), n(exp2, x::lst))
		|PROC (x, exp) -> NL_PROC(n(exp, x::lst))
		|CALL (exp1, exp2) -> NL_CALL(n(exp1, lst), n(exp2, lst))
  
  let translate : program -> nl_program
  =fun pgm -> n(pgm, [])
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
  =fun pgm -> NL_Int 0 (*IGIVEUP...*)
end