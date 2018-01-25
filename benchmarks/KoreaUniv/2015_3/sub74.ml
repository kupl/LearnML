(*Problem 1*)

module Problem1 =struct
type mobile = branch * branch
and branch = SimpleBranch of length * weight
						|CompoundBranch of length * mobile
and length = int
and weight = int

let rec ival : mobile -> int
=fun x->
			match x with
			|(SimpleBranch (a,b),SimpleBranch(c,d))
		  	-> if a*b=c*d then b+d else 0
			|(CompoundBranch (a,b), SimpleBranch (c,d))
				->ival (SimpleBranch (a, ival(b)), SimpleBranch (c,d))
			|(SimpleBranch (a,b), CompoundBranch (c,d))
				->ival (SimpleBranch(a,b), SimpleBranch (c,ival(d)))
			|(CompoundBranch(a,b), CompoundBranch(c,d))
				->ival (SimpleBranch (a,ival(b)), SimpleBranch (c,ival(d)))

let balanced : mobile->bool
=fun (lb, rb) -> if (ival(lb,rb)>0) then true else false
;;

end

(*Problem 2*)
module Problem2 = struct
type exp = V of var
					|P of var * exp
					|C of exp * exp
and var = string

let eval mlist = 
	let rec ival k one_list	=
		 match k with
			|V a -> List.mem a one_list
			|P (a,b)-> ival b(a::one_list)
			|C (a,b) ->ival a one_list && ival b one_list in ival mlist[]

let check : exp -> bool
= fun e -> if (eval e=true)  then true else false
;;

end

(*Problem 3*)
module Problem3=struct
type program = exp
and exp =
		|CONST of int
		|VAR of var
		|ADD of exp * exp
		|SUB of exp * exp
		|ISZERO of exp
		|IF of exp* exp * exp
		|LET of var * exp * exp
		|LETREC of var * var * exp * exp
		|PROC of var * exp
		|CALL of exp * exp
and var = string

type value = Int of int
						|Bool of bool
						|Procedure of var * exp * env
						|RecProcedure of var *var * exp * env
and env = (var*value) list

let empty_env = []
let rec apply_env x e =
				match e with
				|[] -> raise(Failure "This environment is empty!")
				|(a1,a2)::tl->
								if x=a1 then a2 else apply_env x tl
let extend_env (x,v) e = (x,v)::e
						
let rec eval : exp -> env ->  value
=fun exp env->
			match exp with
			|CONST n -> Int n
			|VAR x -> apply_env x env
			|ADD (e1,e2)->
				let v1 = eval e1 env in
				let v2 = eval e2 env in
					(match v1, v2 with
					| Int n1, Int n2 -> Int (n1+n2)
					| _->raise (Failure "Type Error: non-numeric values"))
			|SUB (e1,e2) ->
				let v1 = eval e1 env in
				let v2 = eval e2 env in
				(match v1, v2 with
				|Int n1,Int n2 -> Int(n1-n2)
				|_ -> raise (Failure "Type Error : non-numeric values"))
		|ISZERO e->
				(match eval e env with
				| Int n when n =0 -> Bool true
				|_-> Bool false)
		|IF (e1, e2, e3)->
			(match eval e1 env with
			|Bool true -> eval e2 env
			|Bool false -> eval e3 env
			|_-> raise (Failure "Typer Error : condition must be Bool type"))
		|LET (x, e1, e2)->
		 let v1 = eval e1 env in
			 eval e2 (extend_env (x, v1) env)
		|LETREC (f, x, e1, e2)->
			 eval e2 (extend_env (f, RecProcedure (f,x,e1,env)) env)
		|PROC(x,e) ->
			Procedure (x,e,env)
		|CALL(e1,e2)->
			let v1 = eval e1 env in
			let v2 = eval e2 env in
			(match v1 with
			RecProcedure (f,x,e1,e2)-> eval e1(extend_env( x, v2) (extend_env (f, v1) e2))
		|Procedure (x, e1, e2)->
			eval e1 (extend_env (x,v2) e2)
		|_->raise (Failure "Type Error : expression must be procedure type"))

let run : program -> value
= fun pgm  -> eval pgm empty_env
;;

end
(*Problem 4*)
module Problem4= struct
type program = exp
and exp =
		|CONST of int
		|VAR of var
		|ADD of exp * exp
		|SUB of exp * exp
		|ISZERO of exp
		|IF of exp * exp * exp
		|LET of var * exp * exp
		|PROC of var * exp
		|CALL of exp * exp
and var = string
and rep = var list

let empty_rep = []
let rec apply_rep x e =
		match e with
		|[]->raise (Failure "Representation is empty")
		|h::tl -> if x=h then 0 else 1+apply_rep x tl
let extend_rep x e= x::e


type nl_program = nl_exp
and nl_exp = 
		|NL_CONST of int
		|NL_VAR of int
		|NL_ADD of nl_exp * nl_exp
		|NL_SUB of nl_exp * nl_exp
		|NL_ISZERO of nl_exp
		|NL_IF of nl_exp * nl_exp * nl_exp
		|NL_LET of nl_exp * nl_exp
		|NL_PROC of nl_exp
		|NL_CALL of nl_exp * nl_exp

let rec chg : exp -> rep -> nl_program
=fun exp rep->
		match exp with
		|CONST (n) -> NL_CONST (n)
		|VAR (x) -> NL_VAR(apply_rep x rep)
		|ADD (e1, e2) -> NL_ADD(chg e1 rep, chg e2 rep)
		|SUB(e1, e2) ->NL_SUB (chg e1 rep, chg e2 rep)
		|ISZERO (e) ->NL_ISZERO(chg e rep)
		|IF(e1, e2, e3) -> NL_IF (chg e1 rep, chg e2 rep, chg e3 rep)
		|LET (x, e1, e2)->
			let reps = extend_rep x rep in NL_LET (chg e1 rep, chg e2 reps)
		|PROC (x,e)->
			let reps = extend_rep x rep in 	NL_PROC(chg e reps)
		|CALL(e1, e2) ->
			NL_CALL(chg e1 rep, chg e2 rep)

let rec translate : program -> nl_program
=fun pgm -> chg pgm empty_rep
;;

end
(*Problem 5*)
module Problem5= struct
open Problem4
type nl_value=
			 NL_Int of int
			|NL_Bool of bool
			|NL_Procedure of nl_exp * nl_env
and nl_env = nl_value list
let empty_nl_env = []
let rec apply_nl_env x e=
		match e with
		|[]->raise (Failure "Environment is empty!")
		|h::tl -> if x=0 then h else (apply_nl_env (x-1) tl)
let extend_nl_env x e1 = x::e1

let rec neval : nl_exp -> nl_env -> nl_value
=fun exp env->
		match exp with
		|NL_CONST(n) -> NL_Int(n)
		|NL_VAR(x)->apply_nl_env x env
		|NL_ADD(e1, e2)->
			let v1 = neval e1 env in
			let v2 = neval e2 env in
			(match v1, v2 with
			|NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
			|_->raise(Failure "Type error!"))
		|NL_SUB(e1, e2)->
			let v1 = neval e1 env in
			let v2 = neval e2 env in
			(match v1, v2 with
			|NL_Int n1, NL_Int n2 -> NL_Int (n1-n2)
			|_->raise (Failure "Type error!"))
		|NL_ISZERO(e)->
			(match neval e env with
			|NL_Int k when k=0 -> NL_Bool true
			|_-> NL_Bool false)
		|NL_IF (e1, e2, e3)->
			(match neval e1 env with
			|NL_Bool true -> neval e2 env
			|NL_Bool false -> neval e3 env
			|_-> raise (Failure "Type error!"))
		|NL_LET(e1, e2)->
			let v1 = neval e1 env in neval e2 (extend_nl_env v1 env)
		|NL_PROC(e)-> NL_Procedure (e, env)
		|NL_CALL(e1, e2)->
			let v1 = neval e1 env in
			let v2 = neval e2 env in
				(match v1 with
				|NL_Procedure (e1, e2)->neval e1 (extend_nl_env v2 e2)
				|_-> raise (Failure "Type error!"))

let nl_run : nl_program -> nl_value
=fun pgm -> neval pgm empty_nl_env
;;

end
