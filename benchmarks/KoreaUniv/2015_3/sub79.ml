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

let rec weight : mobile -> int
= fun (lb, rb) -> match (lb, rb) with
	 (SimpleBranch (ll, lw), SimpleBranch (rl, rw)) ->
		lw + rw
	| (SimpleBranch (ll, lw), CompoundBranch (rl, rm)) ->
		lw + (weight rm)
	| (CompoundBranch (ll, lm), SimpleBranch (rl, rw)) ->
		(weight lm) + rw
	| (CompoundBranch (ll, lm), CompoundBranch (rl, rm)) ->
		(weight lm) + (weight rm);;
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match (lb, rb) with
	(SimpleBranch (ll, lw), SimpleBranch (rl, rw)) ->
		(if (ll * lw) = (rl * rw) then
			true else false)
	| (SimpleBranch (ll, lw), CompoundBranch (rl, rm)) ->
		(if (ll * lw) = (rl * (weight rm)) && (balanced rm) then
			true else false)
	| (CompoundBranch (ll, lm), SimpleBranch (rl, rw)) ->
		(if (ll * (weight lm)) = (rl * rw) && (balanced lm) then
			true else false)
	| (CompoundBranch (ll, lm), CompoundBranch (rl, rm)) ->
		(if (ll * (weight lm)) = (rl * (weight rm)) && (balanced lm) &&
			(balanced rm) then
			true else false);;

end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

let rec checklist : string list * string -> bool
= fun (l, s) -> match l with
	[] -> false
	|h::t -> if h = s then true else checklist (t, s);;

let rec extend : exp * string list -> string list
= fun (x, env) -> match x with
	V var -> if (checklist (env, var)) then env else var::env
	| P (v, e) -> extend (e, env)
	| C (e1, e2) -> extend (e1, extend (e2, env));;
  
  let rec check : exp -> bool
  =fun e -> match e with
	V var -> true
	|P(v, e1) -> check e1 && checklist(extend(e1, []), v)
	|C(e1, e2) -> check e1 && check e2;;

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

	let rec eval_bop : (int->int->int)->exp->exp->(var->value)->value
	= fun op e1 e2 env ->
		let v1 = eval e1 env in
		let v2 = eval e2 env in
			(match v1, v2 with
			| Int n1, Int n2 -> Int (op n1 n2)
			| _ -> raise (Failure "Type Error: non-numeric values"))

	and eval : exp -> (var -> value) -> value
	= fun exp env -> match exp with
	|CONST n -> Int n
	|VAR x -> apply_env env x
	|ADD (e1, e2) -> eval_bop (+) e1 e2 env
	|SUB (e1, e2) -> eval_bop (-) e1 e2 env
	|ISZERO e ->
		(let v = eval e env in
			match v with
			| Int n -> if n = 0 then Bool true else Bool false
			| Bool n -> raise (Failure "Type Error")
			| Procedure (_,_,_) -> raise (Failure "Type Error")
			| RecProcedure(_,_,_,_) -> raise (Failure "Type Error"))
	|IF (e1, e2, e3) ->
		(match eval e1 env with
		|Bool true -> eval e2 env
		|Bool false -> eval e3 env
		|_ -> raise (Failure "Type Error: condition must be Bool type"))
	|LET (x, e1, e2) ->
		let v1 = eval e1 env in
			eval e2 (extend_env (x, v1) env)
	|LETREC (f, x, e1, e2) -> eval e2 (extend_env (f, RecProcedure(f, x, e1, env)) env)
	|PROC (v1, e1) -> Procedure(v1, e1, env)
	|CALL (e1, e2) -> (match eval e1 env with
		|Procedure(x, ex, env2) -> eval ex (extend_env (x, eval e2 env) env2)
		|RecProcedure(f, x, ex, env2) -> eval ex (extend_env(f, RecProcedure(f, x, ex, env2)) (extend_env(x, eval e2 env) env2))
		|_ -> raise (Failure "Type Error: expression 1 should be evaluated procedure or RecProcedure"))
  
  let rec run : program -> value
  =fun pgm -> eval pgm (fun x -> empty_env x);;
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

	let rec lexenv : exp -> string list -> string list
	= fun exp l -> match exp with
	|LET (v, e1, e2) -> v::l
	|PROC (v, e) -> v::l
	|_ -> l

	and lookupenv : string -> string list -> int -> int
	= fun v l n-> match l with
	[] -> raise (Failure "Empty environment")
	|x::t -> if x = v then n else lookupenv v t (n+1)

	and convert : program -> string list -> nl_program
	= fun pgm l -> match pgm with
	|CONST n -> NL_CONST n
	|VAR v -> NL_VAR (lookupenv v l 0)
	|ADD (e1, e2) -> NL_ADD(convert e1 l, convert e2 l)
	|SUB (e1, e2) -> NL_SUB(convert e1 l, convert e2 l)
	|ISZERO e1 -> NL_ISZERO (convert e1 l)
	|IF (e1, e2, e3) -> NL_IF(convert e1 l, convert e2 l, convert e3 l)
	|LET (v, e1, e2) -> NL_LET (convert e1 l, convert e2 (lexenv pgm l))
	|PROC (v, e1) -> NL_PROC (convert e1 (lexenv pgm l))
	|CALL (e1, e2) -> NL_CALL(convert e1 l, convert e2 l)

  
  let translate : program -> nl_program
  =fun pgm -> convert pgm [];;

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

let rec nl_lookup_env : nl_value list -> int -> nl_value
	= fun l n -> match l with
	[] -> raise (Failure "Wrong Environment")
	|h::t -> if n = 0 then h else nl_lookup_env t (n-1)

	and nl_op : (int->int->int)->nl_exp->nl_exp->nl_value list->nl_value
	= fun op e1 e2 env->
		let v1 = nl_interpret e1 env in
		let v2 = nl_interpret e2 env in
			(match v1, v2 with
				NL_Int i1, NL_Int i2 -> NL_Int (op i1 i2)
				|_ -> raise (Failure "Type error: value should be NL_Int"))

	and nl_interpret : nl_program -> nl_value list -> nl_value
	= fun exp env -> match exp with
	|NL_CONST i -> NL_Int i
	|NL_VAR i -> nl_lookup_env env i
	|NL_ADD (e1, e2) -> nl_op (+) e1 e2 env
	|NL_SUB (e1, e2) -> nl_op (-) e1 e2 env
	|NL_ISZERO e1 -> (match nl_interpret e1 env with
		NL_Int 0 -> NL_Bool true
		|NL_Int _ -> NL_Bool false
		|_ -> raise (Failure "Type error: value should be NL_Int"))
	|NL_IF (e1, e2, e3) -> (match nl_interpret e1 env with
		|NL_Bool true -> nl_interpret e2 env
		|NL_Bool false -> nl_interpret e3 env
		|_ -> raise (Failure "Type error: expression 1 should be NL_Bool"))
	|NL_LET	(e1, e2) -> nl_interpret e2 ((nl_interpret e1 env)::env)
	|NL_PROC e1 -> NL_Procedure (e1, env)
	|NL_CALL (e1, e2) -> (match nl_interpret e1 env with
		|NL_Procedure (e, env1)->nl_interpret e ((nl_interpret e2 env)::env1)
		|_ -> raise (Failure "Type error: Must call NL_Procedure"))
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> nl_interpret pgm []

end
