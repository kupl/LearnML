
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

	let rec findweight : branch -> int
	=fun (branch) -> match branch with
									|SimpleBranch(a,b) -> b
									|CompoundBranch(a,(bl,br))-> findweight(bl) + findweight(br)



	let rec score : branch -> int
	=fun (branch) -> match branch with
								|SimpleBranch(a,b) -> a*b  (* length*weight *)
								|CompoundBranch(a,(bl,br)) -> a*(findweight(bl)+findweight(br))
								
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) ->match (lb,rb) with
	|(SimpleBranch(a,b),SimpleBranch(c,d)) -> score(lb) = score(rb)
	|(SimpleBranch(a,b),CompoundBranch(c,e)) -> (score(lb) = score(rb)) && balanced(e)
	|(CompoundBranch(c,e),SimpleBranch(a,b)) -> (score(lb) = score(rb)) && balanced(e)
	|(CompoundBranch(a,f),CompoundBranch(c,e)) -> (score(lb) = score(rb)) && (balanced(f) && balanced(e))

end


(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

	let rec inlist : string * string list -> bool (* Is it in list? *)
	=fun (a,l) -> match l with
								|[]-> false
								|hd::tl -> if hd = a then true else inlist(a,tl)



	let rec chlist : exp * string list -> bool (* Check with list*)
	=fun (e,l) -> match e with
							|V a -> inlist(a,l)
							|P(a,env) -> chlist(env,a::l)
							|C(env1,env2) -> chlist(env1,l) && chlist(env2,l)

  let rec check : exp -> bool
  =fun e -> chlist(e,[])
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
			|_ -> raise (Failure "Type Error: non-numeric values"))


	and eval : exp -> env -> value
	=fun exp env -> match exp with
	| CONST a -> Int a
	| VAR s -> apply_env env s
	| ADD (e1, e2) -> eval_bop (+) e1 e2 env
	| SUB (e1, e2) -> eval_bop (-) e1 e2 env
	| ISZERO e -> if (eval e env) = Int 0 then Bool true else Bool false
	| IF (e1,e2,e3) -> if (eval e1 env) = Bool true then eval e2 env else eval e3 env
	| LET (x,e1,e2) -> let v = eval e1 env in eval e2 (extend_env (x,v) env)
  | LETREC (f,x,e1,e2) -> Int 1
	| PROC (v,e) -> Procedure (v, e, env)
	| CALL (e1,e2) -> Int 1


  let rec run : program -> value
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
  
  let translate : program -> nl_program
  =fun pgm -> NL_CONST 0 (* TODO *)
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
  =fun pgm -> NL_Int 0 (* TODO *)
end
