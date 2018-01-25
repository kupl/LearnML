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
  
	let rec calweight : branch -> int
	= fun br -> match br with
						| SimpleBranch (x,y) -> y
						| CompoundBranch (x,y) -> match y with
																		| (a,b) -> calweight a + calweight b
	let callength : branch -> int
	= fun br -> match br with
						| SimpleBranch (x,y) -> x
						| CompoundBranch (x,y) -> x

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> if callength lb * calweight lb = callength rb * calweight rb then (match lb with
				| CompoundBranch (x,y)-> (match rb with
										| CompoundBranch (z,w) -> balanced y && balanced w
										| _ -> balanced y)
				| _ -> (match rb with
								| CompoundBranch (z,w) -> balanced w
								| _ -> true))
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
 
	let rec checklist : string * string list -> bool
	=fun (s,l) -> match l with
								| hd::tl -> if hd = s then true
														else checklist(s,tl)
								| [] -> false

	let rec parse : exp * string list -> bool
	=fun (e,l) -> match e with
								| V a -> checklist(a,l)
								| P (v,e) -> parse(e,(v::l))
								| C (e1,e2) -> parse(e1,l) && parse(e2,l)
	
  let check : exp -> bool
  =fun e -> parse (e,[])

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
	= fun exp env -> match exp with
									| CONST n -> Int n
									| VAR x -> apply_env env x
									| ADD (e1,e2) -> let v1 = eval e1 env in
																	 let v2 = eval e2 env in 
																		(match v1,v2 with
																		| Int n1, Int n2 -> Int (n1 + n2)
																		| _ -> raise (Failure "Type Error: non-numeric values"))
									| SUB (e1,e2) -> let v1 = eval e1 env in
																	 let v2 = eval e2 env in
																		(match v1,v2 with
																		| Int n1, Int n2 -> Int (n1 - n2)
																		| _ -> raise (Failure "Type Error: non-numeric values"))
									| ISZERO e -> (match eval e env with
																| Int n when n = 0 -> Bool true
																| _ -> Bool false)
									| IF (e1,e2,e3) -> (match eval e1 env with
																			| Bool true -> eval e2 env
																			| Bool false -> eval e3 env
																			| _ -> raise (Failure "Type Error: condition must be Bool type"))
									| LET (x,e1,e2) -> let v1 = eval e1 env in
																	eval e2 (extend_env (x,v1) env)
									| LETREC (a,x,e1,e2) -> let f = RecProcedure (a,x,e1,env) in
																	eval e2 (extend_env (a,f) env)
									| PROC (x,e) -> Procedure (x,e,env)
									| CALL (e1,e2) -> match eval e1 env with
																		| Procedure (a,b,c) -> let v = eval e2 env in
																													eval b (extend_env (a,v) c)

																		| RecProcedure (a,b,c,d) -> let v = eval e2 env in
																														eval c (extend_env (b,v) (extend_env (a, (eval e1 env)) d))
									| _ -> raise (Failure "Semantic Error")
  
  
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

	let rec lecaddr : string list * string -> int
	=fun (l,v) -> match l with
							| hd::tl -> if hd = v then 0
													else 1 + lecaddr (tl,v)
							| _ -> 0

	let rec parse : program * string list -> nl_program
	=fun (pgm, l) -> match pgm with
											| CONST n -> NL_CONST n
											| VAR x -> NL_VAR (lecaddr(l,x))
											| ADD (e1,e2) -> NL_ADD (parse(e1,l), parse(e2,l))
											| SUB (e1,e2) -> NL_SUB (parse(e1,l), parse(e2,l))
											| ISZERO e -> NL_ISZERO (parse(e,l))
											| IF (e1,e2,e3) -> NL_IF (parse(e1,l), parse(e2,l), parse(e3,l))
											| LET (x,e1,e2) -> NL_LET (parse(e1,l), parse(e2,x::l))
											| PROC (x,e) -> NL_PROC (parse(e,(x::l)))
											| CALL (e1,e2) -> NL_CALL (parse(e1,l), parse(e2,l))

  let rec translate : program -> nl_program
  =fun pgm -> parse (pgm,[])

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

	let rec cal : int * nl_env -> nl_value
	=fun (i,env) -> if i = 0 then (match env with
																| hd::tl -> hd
																| _ -> raise (Failure "Empty Env"))
									else (match env with
												| hd::tl -> cal ((i-1),tl)
												| _ -> raise (Failure "Empty Env"))
  
	let rec eval : nl_program * nl_env -> nl_value
	=fun (pgm,env) -> match pgm with
										| NL_CONST x -> NL_Int x
										| NL_VAR x -> cal (x,env)
										| NL_ADD (e1,e2) -> let v1 = eval (e1,env) in
																				let v2 = eval (e2,env) in
																				(match v1,v2 with
																				| NL_Int x, NL_Int y -> NL_Int (x+y)
																				| _ -> raise (Failure "Type Error : non-numeric values"))
										| NL_SUB (e1,e2) -> let v1 = eval (e1,env) in
																				let v2 = eval (e2,env) in
																				(match v1,v2 with
																				| NL_Int x, NL_Int y -> NL_Int (x-y)
																				| _ -> raise (Failure "Type Error : non-numeric values"))
										| NL_ISZERO e -> (match eval (e,env) with
																			| NL_Int n when n = 0 -> NL_Bool true
																			| _ -> NL_Bool false)
	
										| NL_IF (e1,e2,e3) -> (match eval (e1,env) with
																					| NL_Bool true -> eval (e2,env)
																					| NL_Bool false -> eval (e3,env)
																					| _ -> raise (Failure "Type Error : condition must be Bool type"))
										| NL_LET (e1,e2) -> let v1 = eval (e1,env) in
																				eval (e2,(v1::env))
										| NL_PROC e -> NL_Procedure (e,env)
										| NL_CALL (e1,e2) -> let v = eval (e1,env) in
																					(match v with
																					| NL_Procedure(a,b) -> let v1 = eval (e2,env) in
																																	eval (a,(v1::b))
																					| _ -> raise (Failure "Semantic Error (CALL)"))

  let nl_run : nl_program -> nl_value
  =fun pgm -> eval (pgm,[])
end
