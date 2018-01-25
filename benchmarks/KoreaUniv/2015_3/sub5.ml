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
  
  let rec balanced : mobile -> bool
=fun (lb,rb) ->
match lb with
| SimpleBranch(l,w) -> (
			let a = l in
			let b= w in
			(
			match rb with
			| SimpleBranch(l,w) -> (let c = l in
						let d = w in
						if ((a*b) = (c*d)) then true else false
						)
			| CompoundBranch(l,m) -> (let c = l in
						let d = (eval m) in
							(
							match d with
							| (k, n) -> if k then (if ((a*b) = (c*n)) then true else false) else false
							)
								
						)
			)
			)
| CompoundBranch(l,m) ->(
			let a = l in
			let b = (eval m) in
			(match b with
			|(x,y) -> if x then 
				(
				match rb with
				| SimpleBranch(l,w) -> (let c = l in
							let d = w in
							if ((a*y) = (c*d)) then true else false
							)
				| CompoundBranch(l,m) -> (let c = l in
							let d = (eval m) in
								(
								match d with
								| (k, n) -> if k then (if ((a*y) = (c*n)) then true else false) else false
								)
								
							)
				)
				else false
			)
			)

and eval: mobile -> (bool * int)
= fun (lb, rb) ->
match lb with
| SimpleBranch(l,w) -> (
			let a =l in
			let b= w in
			(
			match rb with
			| SimpleBranch(l,w) -> (let c = l in
						let d = w in
						if ((a*b) = (c*d)) then (true, (b+d)) else (false, 0)
						)
			| CompoundBranch(l,m) -> (let c = l in
						let d = (eval m) in
							(
							match d with
							| (k, n) -> if k then (if ((a*b) = (c*n)) then (true, b+n) else (false,0)) 
									else (false,0)
							)
								
						)
			)
			)
| CompoundBranch(l,m) ->(
			let a = l in
			(let b = (eval m) in
				(match b with
				| (x,y) -> if x then (match rb with
								| SimpleBranch(l,w) -> (let c = l in
											let d = w in
											if ((a*y) = (c*d)) then (true, (y+d)) else (false, 0)
											)
								| CompoundBranch(l,m) -> (let c = l in
											let d = (eval m) in
												(
												match d with
												| (k, n) -> if k then (if ((a*y) = (c*n)) then (true, (y+n)) else (false,0)) 
														else (false,0)
												)
								
											)
								)
					 else (false,0))
				)
			
				)


end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
let rec check : exp -> bool
=fun e -> 
let a = (eval e []) in
match a with
| true -> true
| false -> false


and eval: exp -> var list -> bool
= fun e env->
match e with
| V x -> lookup x env
| P(x,e1) -> (let v = lookup x env in
		(match v with
		| false -> (eval e1 (extend x env))
		| true -> (eval e1 env)
		))
| C(e1,e2) -> (let a = (eval e1 env) in 
		(match a with 
		| false ->false
		| true -> (eval e2 env)
		))

and lookup: var -> var list -> bool
= fun x env ->
match env with
|[] -> false
| v::tl -> if x = v then true 
	else lookup x tl

and extend: var -> var list -> var list
=fun x env -> x::env
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
=fun op e1 e2 env ->
  let v1 = eval e1 env in
  let v2 = eval e2 env in
    (match v1,v2 with
    | Int n1, Int n2 -> Int (op n1 n2)
    | _ -> raise (Failure "Type Error: non-numeric values"))

and eval : exp -> env-> value
=fun exp env ->
  match exp with
  | CONST n -> Int n
  | VAR x -> apply_env env x
  | ADD (e1,e2) -> eval_bop (+) e1 e2 env
  | SUB (e1,e2) -> eval_bop (-) e1 e2 env
  | ISZERO e ->
    (let v = eval e env in
      match v with
      | Int n -> if n = 0 then Bool true else Bool false
      | _ -> raise(Failure "Type Error: subexpression of zero? must be Int type"))
  | IF (e1,e2,e3) ->
    (match eval e1 env with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise (Failure "Type Error: condition must be Bool type"))
  | LET (x,e1,e2) ->
    (let v1 = eval e1 env in
      eval e2 (extend_env (x, v1) env))
  | LETREC (x,y,e1,e2) -> 
	(eval e2 (extend_env (x, (RecProcedure(x,y,e1,env))) env))
  | PROC(x,e) -> Procedure(x,e,env)
  | CALL(e1,e2) ->
	(let v1 = eval e1 env in
		let v2 = eval e2 env in
		(match v1 with
		| Procedure(x,e,env2) -> (eval e (extend_env (x, v2) env2))
		| RecProcedure(x,y,e,env2) -> (eval e ((extend_env (y, v2) (extend_env (x, v1) env2))))
		| _ -> raise(Failure "Type Error: subexpression of CALL must be Procedure or RecProcedure type")
		)
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
  
let rec transform: exp -> var list -> nl_exp 
= fun e env -> 
match e with
| CONST n -> NL_CONST n
| VAR x -> NL_VAR (find x env)
| ADD (e1,e2) -> NL_ADD((transform e1 env), (transform e2 env))
| SUB (e1,e2) -> NL_SUB((transform e1 env), (transform e2 env))
| ISZERO e -> NL_ISZERO(transform e env)
| IF(e1,e2,e3) -> NL_IF((transform e1 env), (transform e2 env), (transform e3 env))
| LET(x,e1,e2) -> NL_LET((transform e1 env), (transform e2 (x::env)))
| PROC(x,e) -> NL_PROC(transform e (x::env))
| CALL(e1,e2) -> NL_CALL((transform e1 env), (transform e2 env))


and find : var -> var list -> int
= fun x l ->
match l with
| hd::tl -> if hd = x then 0
	    else ((find x tl)+1)
| _ -> raise (Failure "error: can not find matching variable")


  let translate : program -> nl_program
  =fun pgm -> transform pgm []
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
  
let rec eval: nl_exp -> nl_env -> nl_value
= fun e env ->
match e with 
| NL_CONST n -> NL_Int n
| NL_VAR n -> (search n env)
| NL_ADD (e1,e2) -> let v1 = eval e1 env in 
			let v2 = eval e2 env in
			(match v1, v2 with
			| NL_Int n1, NL_Int n2 ->NL_Int (n1+n2)
			| _ -> raise (Failure "Type Error: non-numeric values")
			)
| NL_SUB (e1,e2) -> let v1 = eval e1 env in 
			let v2 = eval e2 env in
			(match v1, v2 with
			| NL_Int n1, NL_Int n2 ->NL_Int (n1-n2)
			| _ -> raise (Failure "Type Error: non-numeric values")
			)
| NL_ISZERO e -> (match eval e env with
		 | NL_Int n when n = 0 -> NL_Bool true
		 | _ -> NL_Bool false
		 )
| NL_IF (e1,e2,e3) -> (match eval e1 env with
			| NL_Bool true -> eval e2 env
			| NL_Bool false -> eval e3 env
			| _-> raise(Failure "Type Error: condition must be Bool type")
		      )
| NL_LET (e1,e2) -> let v1 = eval e1 env in 
			eval e2 (v1::env)
| NL_PROC e -> NL_Procedure(e,env)
| NL_CALL (e1,e2) -> (let v1 = eval e1 env in
			let v2 = eval e2 env in
			(match v1 with
			| NL_Procedure(e,env1) -> eval e (v2::env1)
			| _ -> raise (Failure "Wrong operator of NL_CALL")
			)
		     )

and search: int -> nl_env -> nl_value
= fun n l ->
match l with
| hd::tl -> if n = 0 then hd
	    else (search (n-1) tl)
| _ -> raise (Failure "The corresponding value to the variable doesn't exists")

  
  let nl_run : nl_program -> nl_value
  =fun pgm -> eval pgm []
end