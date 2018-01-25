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
  
  let rec search n elist =
	match elist with
	 | [] -> raise (Failure "empty environment")
	 | hd::tl -> if n = 0 then hd else search (n-1) tl
  
  let rec nl_eval : nl_exp -> nl_env -> nl_value
	=fun nl_exp nl_env ->
	match nl_exp with
	 | NL_CONST n -> NL_Int n
	 | NL_VAR n -> 
		(match nl_env with
		 | [] -> raise (Failure "no matching values")
		 | hd::tl -> if n = 0 then hd else (nl_eval (NL_VAR (n-1)) tl)
		)
	 | NL_ADD (e1,e2) -> 
		let v1 = nl_eval e1 nl_env in
		let v2 = nl_eval e2 nl_env in
			(match v1,v2 with
			 | NL_Int n1, NL_Int n2 -> NL_Int (n1 + n2)
			 | _ -> raise (Failure "Type Error: non-numeric values")
			)
	 | NL_SUB (e1,e2) ->
		let v1 = nl_eval e1 nl_env in
		let v2 = nl_eval e2 nl_env in
			(match v1,v2 with
			 | NL_Int n1, NL_Int n2 -> NL_Int (n1 - n2)
			 | _ -> raise (Failure "Type Error: non-numeric values")
			)
	 | NL_ISZERO e ->  
		(match nl_eval e nl_env with
		 | NL_Int n when n = 0 -> NL_Bool true
		 | _ -> NL_Bool false
		)
	 | NL_IF (e1,e2,e3) ->
		 (match nl_eval e1 nl_env with
		   | NL_Bool true -> nl_eval e2 nl_env
		   | NL_Bool false -> nl_eval e3 nl_env
		   | _ -> raise (Failure "Type Error: condition must be Bool type")
		  )
	 | NL_LET (e1,e2) ->
		let v1 = nl_eval e1 nl_env in
			nl_eval e2 ([v1]@nl_env) 
	 | NL_PROC (e) -> NL_Procedure (e,nl_env)
	 | NL_CALL (e1,e2) -> 
	 (match nl_eval e1 nl_env with
	   | NL_Procedure (e,nl_env') ->
			let v = nl_eval e2 nl_env in
				nl_eval e ([v]@nl_env')
	   | _ -> raise (Failure "Type Error: expression1 must be Procedure type")
	  )
	  (*
  let pgm = LET ("x", CONST 1, VAR "x")
  
  let test = CALL(LET ("x", CONST 37, PROC("y", LET ("z", SUB (VAR "y",VAR "x"), SUB (VAR "x", VAR "y")))),CONST 10)
		*)
  let nl_run : nl_program -> nl_value
  =fun pgm -> nl_eval pgm []
