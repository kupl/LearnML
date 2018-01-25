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
  
  let rec total_weight : branch -> int
	= fun b -> match b with
	SimpleBranch (l, w) -> w
	|CompoundBranch (l, m) ->
		(match m with
		(b1, b2) -> (total_weight b1) + (total_weight b2))

	let rec  balanced : mobile -> bool
	=fun (lb,rb) -> match (lb, rb) with
	(SimpleBranch (l1, w1), SimpleBranch(l2, w2)) ->
		if (l1 * w1) = (l2 * w2) then true else false
	|(CompoundBranch (l1, m1), SimpleBranch(l2, w2)) ->
		if l1 * (total_weight (CompoundBranch(l1, m1))) = (l2 * w2) then 
		balanced m1 else false
	|(SimpleBranch (l1, w1), CompoundBranch(l2, m2)) ->
		if (l1 * w1) = l2 * (total_weight(CompoundBranch(l2, m2))) then
		balanced m2 else false
	|(CompoundBranch (l1, m1), CompoundBranch(l2, m2)) ->
		if l1 * (total_weight (CompoundBranch(l1, m1))) = l2 * (total_weight
		(CompoundBranch(l2, m2))) then (balanced m1) && (balanced m2) else false
end



(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
	let rec change : (var * exp) -> exp
	= fun (v,e) -> match e with
	V "" -> raise(Failure "error")
	|V v' -> V v'
	|P(v', e') -> change (v', e')
	|C(e1, e2) -> if (V v) = e1 then (if (V v) = e2 then change(v, e1) 
		else change(v, e2)) else (if (V v) = e2 then change(v, e1) 
		else change(v, V v))

  let check : exp -> bool
  =fun e -> match e with
	V "" -> raise(Failure "error")
	|V v -> false
	|P(v, e') -> if (V v) = (change (v, e')) then true else false
	|C(e1, e2) -> false
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
  
	let rec eval_bop : (int -> int -> int) -> exp -> exp -> env -> value
	= fun op e1 e2 env ->
	let v1 = eval e1 env in
	let v2 = eval e2 env in
		(match v1, v2 with
		| Int n1, Int n2 -> Int (op n1 n2)
		| _ -> raise(Failure "Type Error: non-numeric values"))

	and eval : exp -> env -> value
	= fun exp env -> match exp with
	CONST n -> Int n
	|VAR x -> apply_env env x
	|ADD (e1, e2) -> eval_bop (+) e1 e2 env
	|SUB (e1, e2) -> eval_bop (-) e1 e2 env
	|ISZERO e ->
		(let v = eval e env in
			match v with
			| Bool _ -> raise(Failure "Type Error: subexpression of zero? must be Int type")
			| Int n -> if n = 0 then Bool true else Bool false
			| _ -> raise(Failure "Type Error: subexpression of zero? must be Int type"))
	|IF(e1,e2,e3) ->
		(match eval e1 env with
		|Bool true -> eval e2 env
		|Bool false -> eval e3 env
		|_ -> raise(Failure "Type Error: condition must be Bool type"))
	|LET (x, e1, e2) ->
		let v1 = eval e1 env in
			eval e2 (extend_env (x, v1) env)
	|LETREC (f, x, e1, e2) ->
		eval e2 (extend_env (f, RecProcedure(f,x,e1,env)) env)
	|PROC (x, e) -> Procedure(x,e,env)
	|CALL (e1, e2) -> 
		(match e1 with
			|VAR f ->
				let v1 = eval e1 env in (match v1 with
					|Procedure(x,e,env2) ->
						let v = eval e2 env in
							eval e (extend_env (x, v) env2)
					|RecProcedure(f,x,e3,env2) ->
						let v = eval e2 env in
							eval e3 (extend_env (x, v) (extend_env (f, RecProcedure(f,x,e3,env2)) env2))
					|_ -> raise(Failure "Type Error: not function call"))
			|_ -> raise(Failure "Type Error: not function call"))

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
  
	let save : var list -> exp -> var list
	= fun l e -> match e with
	VAR x -> x::l
	|_ -> raise(Failure "Error") 
	
	let rec find : var -> var list -> int
	= fun x l -> match l with
		|[] -> 0
		|h::t -> if x = h then 0 else (find x t) + 1

	let rec help : exp -> var list -> nl_exp
	= fun e l -> match e with
		|CONST n -> NL_CONST n
		|VAR x -> NL_VAR (find x l)
		|ADD(e1,e2) -> NL_ADD(help e1 l, help e2 l)
		|SUB(e1,e2) -> NL_SUB(help e1 l, help e2 l)
		|ISZERO e -> NL_ISZERO(help e l)
		|IF(e1,e2,e3) -> NL_IF(help e1 l, help e2 l , help e3 l)
		|LET(v,e1,e2) -> NL_LET(help e1 l, help e2 (save l (VAR v)))
		|PROC(v,e) -> NL_PROC(help e (save l (VAR v)))
		|CALL(e1,e2) -> NL_CALL(help e1 l, help e2 l)

	
  let rec translate : program -> nl_program
  =fun pgm -> help pgm [] 
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

	let rec rematch : (int * nl_env) -> nl_value
	= fun (n, l) -> match (n, l) with
		|(_, []) -> raise(Failure "Type Error: no value")
		|(0, h::t) -> h
		|(n', h::t) -> rematch (n'-1, t)

	let rec eval_bop : (int -> int -> int) -> nl_exp -> nl_exp -> nl_env -> nl_value
	=fun op e1 e2 env -> 
	let v1 = eval e1 env in
	let v2 = eval e2 env in
		(match v1, v2 with
		| NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
		| _ -> raise(Failure "Type Error"))
  
	and eval : nl_exp -> nl_env -> nl_value
	= fun nl_exp env -> match nl_exp with
		|NL_CONST n -> NL_Int n
		|NL_VAR n -> rematch(n, env)
		|NL_ADD(nl_e1, nl_e2) -> eval_bop (+) nl_e1 nl_e2 env
		|NL_SUB(nl_e1, nl_e2) -> eval_bop (-) nl_e1 nl_e2 env
		|NL_ISZERO nl_e ->
			(let v = eval nl_e env in
				match v with
				|NL_Bool _ -> raise(Failure "Type Error")
				|NL_Int n -> if n = 0 then NL_Bool true else NL_Bool false
				|_ -> raise(Failure "Type Error"))
		|NL_IF(nl_e1, nl_e2, nl_e3) -> 
			(match eval nl_e1 env with
			|NL_Bool true -> eval nl_e2 env
			|NL_Bool false -> eval nl_e3 env
			|_ -> raise(Failure "Type Error")) 
		|NL_LET(nl_e1, nl_e2) ->
			let v1 = eval nl_e1 env in
				eval nl_e2 (v1::env)
		|NL_PROC(nl_e) -> NL_Procedure(nl_e, env)
		|NL_CALL(nl_e1, nl_e2) ->
			(match nl_e1 with
			NL_VAR n ->
				let v1 = eval nl_e1 env in (match v1 with
					|NL_Procedure(nl_e, env2) -> 
						let v = eval nl_e2 env in
							eval nl_e (v::env2)
					|_ -> raise(Failure "not function call"))
			|_ -> raise(Failure "Type Error: not function call"))

  let nl_run : nl_program -> nl_value
  =fun pgm -> eval pgm []
end
