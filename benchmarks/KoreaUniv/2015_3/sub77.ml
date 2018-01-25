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


	let rec scale : branch -> int
	=fun branch ->
		match branch with 
		 | SimpleBranch (len, x) -> x
		 | CompoundBranch (len, (br1,br2)) -> scale br1 + scale br2

	let rec balanced : mobile -> bool 
	=fun (lb,rb) -> 
		match lb with
		 | SimpleBranch (len1, x1) -> 
			(match rb with
			 | SimpleBranch (len2, x2) -> if (len1 * (scale lb) = len2 * (scale rb)) then true else false
			 | CompoundBranch (len3, mobile3) -> if (balanced mobile3 = true) && (len1 * (scale lb) = len3 * (scale rb)) then true else false
			)
		 | CompoundBranch (len5, mobile5) -> 
			(match rb with
			 | SimpleBranch (len6, x6) -> if (balanced mobile5 = true) && (len5 * (scale lb) = len6 * (scale rb)) then true else false
			 | CompoundBranch (len7, mobile7) -> if (balanced mobile5 = true) && (balanced mobile7 = true) && 
													(len5 * (scale lb) = len7 * (scale rb)) then true else false
			)
	(*
	let test = (CompoundBranch (3,
	(CompoundBranch (2, (SimpleBranch (1, 1), SimpleBranch (1, 1))),
	SimpleBranch (1, 4))),
	SimpleBranch (6, 3))
	*)
end
(***********************************)
(**            Problem 2          **)
(***********************************)
module Problem2 = struct
	type exp = V of var
			 | P of var * exp
			 | C of exp * exp
	and var = string

	let rec elist : exp -> var list
	= fun exp ->
		match exp with
		 | V var -> []
		 | P (var, exp1) -> 
		  (match exp1 with
		   | V var2 -> [var]
		   | P (var2, exp2) -> [var;var2]@(elist exp2)
		   | C (exp3, exp4) -> [var]@(elist exp3)@(elist exp4)
		  )
		 | C (exp2, exp3) -> 
		  (match exp2 with
		   | V var -> elist exp3
		   | P (var2, exp4) -> [var2]@(elist exp4)
		   | C (exp5, exp6) -> (elist exp5)@(elist exp6)@(elist exp3)
		  )

	let rec vlist exp =
		match exp with
		 | V var -> [var]
		 | P (var, exp1) -> 
		  (match exp1 with
		   | V var2 -> [var2]
		   | P (var2, exp2) -> vlist exp2
		   | C (exp3, exp4) -> (vlist exp3)@(vlist exp4)
		  )
		 | C (exp2, exp3) -> 
		  (match exp2 with
		   | V var -> [var]@(vlist exp3)
		   | P (var2, exp4) -> vlist exp4
		   | C (exp5, exp6) -> (vlist exp5)@(vlist exp6)@(vlist exp3)
		  )
		  
		  
	let rec exist explist varlist =
		match varlist with
		 | [] -> true
		 | vhd::vtl -> 
		  (match explist with
		   | [] ->  false
		   | ehd::etl ->  if vhd = ehd then exist explist vtl 
						  else exist etl [vhd] || exist etl varlist 
		  )
		  
	let rec check : exp -> bool
	=fun exp ->
	 exist (elist exp) (vlist exp)
	 
	 (*
	 
let test1 = P ("a", V "a")
let test2 = P ("a", P ("a", V "a"))
let test3 = P ("a", P ("b", C (V "a", V "b")))
let test4 = P ("a", C (V "a", P ("b", V "a")))
let test5 = P ("a", V "b")
let test6 = P ("a", C (V "a", P ("b", V "c")))
let test7 = P ("a", P ("b", C (V "a", V "c")))
let test8 = P ("c", P ("a", P ("b", C (V "a", V "c"))))
	 *)

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
	 | ADD (e1,e2) ->
		let v1 = eval e1 env in
		let v2 = eval e2 env in
			(match v1,v2 with
			 | Int n1, Int n2 -> Int (n1 + n2)
			 | _ -> raise (Failure "Type Error: non-numeric values")
			)
	 | SUB (e1,e2) ->
		let v1 = eval e1 env in
		let v2 = eval e2 env in
			(match v1,v2 with
			 | Int n1, Int n2 -> Int (n1 - n2)
			 | _ -> raise (Failure "Type Error: non-numeric values")
			)
	 | ISZERO e ->
	  (match eval e env with
	   | Int n when n = 0 -> Bool true
	   | _ -> Bool false
	  )
	 | IF (e1,e2,e3) ->
	  (match eval e1 env with
	   | Bool true -> eval e2 env
	   | Bool false -> eval e3 env
	   | _ -> raise (Failure "Type Error: condition must be Bool type")
	  )
	 | LET (x,e1,e2) ->
		let v1 = eval e1 env in
			eval e2 (extend_env (x,v1) env)
	 | LETREC (f,x,e1,e2) ->
			eval e2 (extend_env (f,(RecProcedure (f,x,e1,env))) env)
	 | PROC (x,e) -> Procedure (x,e,env)
	 | CALL (e1,e2) -> 
	  (match eval e1 env with
	   | Procedure (x,e,env') -> 
		 let v = eval e2 env in
			eval e (extend_env (x,v) env')
	   | RecProcedure (f,x,e,env') -> 
		 let v = eval e2 env in
			eval e (extend_env (f,(RecProcedure (f,x,e,env'))) (extend_env (x,v) env')) 
	   | _ -> raise (Failure "Type Error: call must be Procedure or RecProcedure type")
	  )

let run : program -> value
=fun pgm -> eval pgm empty_env
		 	  
	  
	 (*
		let pgm = LETREC ("double", "x", IF (ISZERO (VAR "x"), CONST 0, ADD (CALL (VAR "double", SUB (VAR "x", CONST 1)) ,
		CONST 2)), CALL (VAR "double", CONST 6))
	*)

		 
		 
		 

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
  
  let rec rank elist x =
	match elist with
	 | [] -> 0  
	 | hd::tl -> if hd = x then 0
				else 1 + rank tl x
 
	type elist = nl_exp list
  
  let rec translatewe pgm elist = 
	match pgm with
	 | CONST n -> NL_CONST n
	 | VAR x -> NL_VAR (rank elist x) 
	 | ADD (e1,e2) -> NL_ADD ((translatewe e1 elist) , (translatewe e2 elist))
	 | SUB (e1,e2) -> NL_SUB ((translatewe e1 elist), (translatewe e2 elist))
	 | ISZERO e -> NL_ISZERO (translatewe e elist)
	 | IF (e1,e2,e3) -> NL_IF ((translatewe e1 elist), (translatewe e2 elist), (translatewe e3 elist))
	 | LET (x,e1,e2) -> NL_LET ((translatewe e1 elist), (translatewe e2 ([x]@elist)))
	 | PROC (x,e) -> NL_PROC (translatewe e ([x]@elist))
	 | CALL (e1,e2) -> NL_CALL ((translatewe e1 elist), (translatewe e2 elist))

  
  let rec translate : program -> nl_program
  =fun pgm -> translatewe pgm []



  (* 
  let pgm = LET ("x", CONST 37,PROC ("y", LET ("z", SUB (VAR "y", VAR "x"),SUB (VAR "x", VAR "y")))) 
  *)
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
  
end