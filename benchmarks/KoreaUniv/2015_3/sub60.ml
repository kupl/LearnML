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
 
  let rec bal : mobile -> int
  =fun (lb,rb) -> match lb,rb with
  SimpleBranch(l1,w1),SimpleBranch(l2,w2)-> w1+w2
  |CompoundBranch(l1,m1),SimpleBranch(l2,w2) -> bal(m1)+w2
  |SimpleBranch(l1,w1),CompoundBranch(l2,m) -> w1+bal(m)
  |CompoundBranch(l1,m1),CompoundBranch(l2,m2) -> bal(m1)+bal(m2)
  
 let rec balanced : mobile -> bool
  =fun(lb,rb) -> match lb,rb with
  SimpleBranch(l1,w1),SimpleBranch(l2,w2) -> if(l1*w1)!=(l2*w2)
   then false else true
  |CompoundBranch(l1,m1),SimpleBranch(l2,w2) -> if balanced(m1) then (if((bal(m1)*l1)=(l2*w2))
   then true else false) else false
  |SimpleBranch(l1,w1),CompoundBranch(l2,m) -> if balanced(m) then(if((l1*w1)=(bal(m)*l2))
   then true else false) else false
  |CompoundBranch(l1,m1),CompoundBranch(l2,m2) ->if balanced(m1)&&balanced(m2) 
then( if((l1*bal(m1))=(l2*bal(m2))) then true else false) else false

end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
 let rec ch : exp*var -> bool 
=fun(e,a) -> match e with
V b -> if a=b then true else false
|P(b,e) -> (match e with 
						V c -> if (c=b||c=a) then true else false
						|P(c,e) -> ch(e,c)||ch(e,b)||ch(e,a)
						|C(e1,e2) -> if(a=b) then ch(e1,a)&&ch(e2,a)
						else (ch(e1,a)||ch(e1,b))&&(ch(e2,a)||ch(e2,b)))
|C(e1,e2) -> ch(e1,a)&&ch(e2,a)

  let check : exp -> bool
  =fun e ->match e with 
	V a-> false
	|P(a,e) -> ch(e,a)
	|C(e1,e2) ->false
	
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
  
let rec eval : exp*env -> value
=fun (exp,ev) -> match exp with
CONST n -> Int n
|VAR x -> apply_env ev x
|ADD (e1,e2) ->let a1= eval(e1,ev)
							in let a2=eval(e2,ev)
							in (match a1,a2 with
							|Int n1,Int n2 -> Int (n1+n2)
							|_ -> raise(Failure"error"))
|SUB (e1,e2) ->let a1=eval(e1,ev)
						in let a2=eval(e2,ev)
					in (match a1, a2 with
					|Int n1, Int n2 -> Int (n1-n2)
					|_-> raise(Failure "error"))
|ISZERO e ->let a=eval(e,ev) in (match a with
					|Int n -> if n=0 then Bool true else Bool false
					|_ -> raise(Failure "error"))
|IF (e1,e2,e3) ->let a=eval(e1,ev)
					in (match a with
					|Bool true -> eval(e2,ev)
					|Bool false -> eval(e3,ev)
					|_-> raise(Failure "error"))
|LET (x,e1,e2) -> let v1 = eval(e1,ev)
								in eval(e2, (extend_env(x,v1) ev))
|LETREC (f,x,e1,e2) ->eval (e2, (extend_env(f, RecProcedure(f,x,e1,ev)) ev))
|PROC (x,e) -> Procedure (x,e,ev)
|CALL (e1,e2) ->let p = eval(e1,ev)
							in (match p with
						|Procedure(x,e,ev1)->(
						let v1= eval(e2,ev)
						 in eval(e,(extend_env(x,v1) ev1)))
						|RecProcedure(f,x,e,ev1)-> let v=eval(e2,ev) in
eval(e,(extend_env (x,v) (extend_env(f,p) ev1)))
						|_-> raise(Failure "error"))

  let run : program -> value
  =fun pgm -> eval (pgm, empty_env)

let p1=LETREC ("double", "x", IF(ISZERO(VAR "x"),
CONST 0, ADD(CALL (VAR "double", SUB(VAR "x", CONST 1)),
CONST 2)), CALL(VAR "double", CONST 6));;

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
let rec scope:var list*var*int->int
=fun(env,v,n)->match env with 
|[] -> raise(Failure "no variable")
|hd::tl -> if (hd=v) then n else scope(tl,v,n+1)
 
let rec trans: exp*var list ->nl_exp 
=fun (exp, env)-> match exp with
|CONST n -> NL_CONST n
|VAR x -> NL_VAR (scope (env, x, 0))
|ADD (e1, e2) -> let a1= trans(e1,env) 
							in let a2= trans(e2,env)
							in NL_ADD(a1,a2)
|SUB (e1, e2) -> let a1=trans(e1,env)
							in let a2=trans(e2,env)
							in NL_SUB(a1,a2)
|ISZERO e -> NL_ISZERO(trans(e,env))
|IF (e1,e2,e3) ->NL_IF(trans(e1,env),trans(e2,env),trans(e3,env))
|LET (x,e1,e2)->NL_LET(trans(e1,env),trans(e2,x::env))
|PROC(x,e)->NL_PROC(trans(e,x::env))
|CALL(e1,e2)->NL_CALL(trans(e1,env),trans(e2,env))
 
  let translate : program -> nl_program
  =fun pgm -> trans(pgm,[])
let pgm1 = LET("x",CONST 37, PROC("y",LET("z", SUB(VAR "y", VAR "x"), SUB(VAR "x", VAR "y"))));;

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
 
let rec nth : nl_env*int -> nl_value
= fun (l,n) -> match l with 
|[] -> raise(Failure "error")
|hd::tl -> if(n=0) then hd else nth(tl,n-1)

let rec eval : nl_program*nl_env -> nl_value 
=fun (pro,env) -> match pro with
|NL_CONST n -> NL_Int n
|NL_VAR x -> nth(env,0)
|NL_ADD (e1,e2) -> let a1 =eval(e1,env)
								in let a2=eval(e2,env)
								in (match a1,a2 with
								|NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
								|_->raise(Failure"error"))
|NL_SUB(e1,e2)->let a1=eval(e1,env)
							in let a2=eval(e2,env)
							in (match a1,a2 with
							|NL_Int n1,NL_Int n2 -> NL_Int (n1-n2)
							|_->raise(Failure("error")))
|NL_ISZERO e ->let a=eval(e,env) in (match a with
							|NL_Int n -> if n=0 then NL_Bool true else NL_Bool false
							|_->raise(Failure("error")))
|NL_IF(e1,e2,e3)->let a=eval(e1,env) in (match a with
							|NL_Bool true -> eval(e2,env)
							|_-> eval(e3,env))
|NL_LET(e1,e2) -> let v1 =eval(e1,env) in
									eval(e2,v1::env)
|NL_PROC(e) -> NL_Procedure(e,env)
|NL_CALL(e1,e2) -> let p = eval(e1,env) in 
								let v=eval(e2,env) in (match p with
								|NL_Procedure( e,ev1) -> eval(e,v::ev1)
								|_-> raise(Failure "error"))						
	
  let nl_run : nl_program -> nl_value
  =fun pgm -> eval(pgm,[])

end
