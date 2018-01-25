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

(*if balnaced return (weight,len) if not weight is -1*)
							(* len & weight *)
let rec calweight : branch -> (int * int)
= fun br ->
	match br with
	| SimpleBranch (len, weight) -> (len, weight)
	| CompoundBranch (len, mob) ->
		(match mob with
		 | (br1, br2) ->
			let (ll, lw) = calweight(br1)
				in let (rl, rw) = calweight(br2)
					in if ll*lw = rl*rw then (len, lw+rw)
					  else (len, -1)
		)

let balanced : mobile -> bool
=fun (lb,rb) -> 
	let (len1, wei1) = (calweight lb)
		in let (len2, wei2) = calweight rb
			in if len1 * wei1 = len2 * wei2 then true
			   else false
end

(* open Problem1;;

balanced (CompoundBranch (3,
						(CompoundBranch (2, 
										(SimpleBranch (1, 1), SimpleBranch (1, 1))),
										 SimpleBranch (1, 4))),
						SimpleBranch (6, 3));;
balanced (CompoundBranch (3, 
						 (SimpleBranch(1,2), SimpleBranch(1,2))), 
						 SimpleBranch(12,1));; *)


(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct

type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string


let rec chk : exp -> string list -> bool
= fun e env ->
	match e with
	| V (a) -> if findInEnv a env then true else false
	| P (v, exp) -> chk exp (v::env)
	| C (e1, e2) -> (chk e1 env) && (chk e2 env)
	
and findInEnv : string -> string list -> bool
= fun var env ->
	match env with
	| [] -> false
	|hd::tl -> if var = hd then true else findInEnv var tl


let check : exp -> bool
=fun e -> (* TODO *)
	chk e []

end
(* 
open Problem2;;
check (P ("a", V "a"));;
check (P ("a", P ("a", V "a")));;
check (P ("a", P ("b", C (V "a", V "b"))));;
check (P ("a", C (V "a", P ("b", V "a"))));;

check (P ("a", V "b"));;
check (P ("a", C (V "a", P ("b", V "c"))));;
check (P ("a", P ("b", C (V "a", V "c"))));; *)



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


let rec _run : program -> env -> value
= fun pgm en->
	match pgm with
	| CONST (i) -> Int (i)
	| VAR (v) -> apply_env en v
	| ADD (e1, e2) -> 
		(match ((_run e1 en), (_run e2 en)) with
		| (Int (i1), Int (i2)) -> Int (i1 + i2) 
		|_ -> raise (Failure "error"))
	| SUB (e1, e2) -> 
		(match ((_run e1 en), (_run e2 en)) with
		| (Int (i1), Int (i2)) -> Int (i1 - i2)
		|_ -> raise (Failure "error"))
	| ISZERO (e) -> if (_run e en) = (Int (0)) then Bool(true) else Bool(false)
	| IF (e1, e2, e3) -> if (_run e1 en) = (Bool (true)) 
					  then _run e2 en
				      else _run e3 en
	| LET (v, e1, e2) -> _run e2 (extend_env (v, (_run e1 en)) en)
	| LETREC (f, x, e1, e2) -> _run e2 (extend_env (f, RecProcedure (f, x, e1, en)) en)
	| PROC (v, e) -> Procedure (v, e, en)
	| CALL (e1, e2) ->
		let temp = (_run e1 en) in
			match temp with
			| Procedure (v, e, _env) -> _run e (extend_env (v, (_run e2 en)) _env)
			| RecProcedure (f, x, e, _env) -> _run e (extend_env (x, (_run e2 en)) (extend_env (f, RecProcedure (f, x, e, _env)) _env))
			|_ -> raise (Failure "error")

let run : program -> value
=fun pgm ->(* TODO *)
	_run pgm empty_env
end
(* 
open Problem3;;
let pgm = LETREC ("double",
					"x",
					IF (ISZERO (VAR "x"), 
						CONST 0, 
						ADD (CALL (VAR "double", SUB (VAR "x", CONST 1)), CONST 2)),
					CALL (VAR "double", CONST 6))
;;
run pgm;;  *)


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

type indexBook = var list
let extendIndexBook : var -> indexBook -> indexBook
= fun v iBook -> (v::iBook)


let rec _getIndex : var -> indexBook -> int -> int
= fun v iBook currIndex ->
	match iBook with
	| [] -> raise (Failure "No such variable in Index Book")
	| hd::tl -> if (List.hd iBook) = v then currIndex 
		  else  (_getIndex v tl (currIndex+1))

let getIndex : var -> indexBook -> int
= fun v iBook ->
	_getIndex v iBook 0

let rec _trans : program -> indexBook -> nl_program
= fun pgm iBook ->
	match pgm with
	| CONST (i) -> NL_CONST (i)
	| VAR (v) -> NL_VAR (getIndex v iBook)
	| ADD (e1, e2) -> NL_ADD((_trans e1 iBook), (_trans e2 iBook))
	| SUB (e1, e2) -> NL_SUB((_trans e1 iBook), (_trans e2 iBook))
	| ISZERO (e) -> NL_ISZERO (_trans e iBook)
	| IF (e1, e2, e3) -> NL_IF ((_trans e1 iBook), (_trans e2 iBook), (_trans e3 iBook)) 
	| LET (v, e1, e2) -> NL_LET( (_trans e1 iBook), (_trans e2 (extendIndexBook v iBook)))
	| PROC (v, e) -> NL_PROC (_trans e (extendIndexBook v iBook))
	| CALL (e1, e2) -> NL_CALL ((_trans e1 iBook), (_trans e2 iBook))

let translate : program -> nl_program
=fun pgm -> 
	_trans pgm []

end
(* 
open Problem4;;
translate (LET ("x",
				CONST 37,
				PROC ("y", 
					  LET ("z",
						   SUB (VAR "y", VAR "x"),
						   SUB (VAR "x", VAR "y")))))
;; *)


(***********************************)
(**            Problem 5          **)
(***********************************)

module Problem5 = struct
open Problem4
type nl_value = NL_Int of int 
              | NL_Bool of bool 
              | NL_Procedure of nl_exp * nl_env
and nl_env = nl_value list

let rec getValue : int -> nl_env -> nl_value
= fun i env ->
	match env with
	| [] -> raise (Failure "Empty environment")
	| hd::tl -> if i = 0 then hd else getValue (i-1) tl


let add_NL_Int
= fun ni1 ni2 ->
		match ni1 with 
		|NL_Int (a) ->	
			(match ni2 with 
			 |NL_Int (b) -> NL_Int(a + b)
			 |_ -> raise (Failure "error"))
		|_ -> raise (Failure "error")

let sub_NL_Int
= fun ni1 ni2 ->
		match ni1 with 
		|NL_Int (a) ->	
			(match ni2 with 
			 |NL_Int (b) -> NL_Int(a - b)
			 |_ -> raise (Failure "error"))
		|_ -> raise (Failure "error")


let rec _nl_run : nl_program -> nl_env -> nl_value
= fun pgm env ->
	match pgm with
	| NL_CONST (i) -> NL_Int (i)
	| NL_VAR (i) -> (getValue i env)
	| NL_ADD (e1, e2) -> add_NL_Int (_nl_run e1 env) (_nl_run e2 env)
	| NL_SUB (e1, e2) -> sub_NL_Int (_nl_run e1 env) (_nl_run e2 env)
	| NL_ISZERO (e) -> if (_nl_run e env) = NL_Int (0) then NL_Bool (true)
					   else NL_Bool (false)
	| NL_IF (e1, e2, e3) -> if (_nl_run e1 env) = NL_Bool (true) 
								then _nl_run e2 env 
								else _nl_run e3 env
	| NL_LET (e1, e2) -> _nl_run e2 ((_nl_run e1 env)::env)
	| NL_PROC (e) -> NL_Procedure (e, env)
	| NL_CALL (e1, e2) ->
			let NL_Procedure (procExp, procEnv) = _nl_run e1 env
			in _nl_run procExp ((_nl_run e2 env)::procEnv)

let nl_run : nl_program -> nl_value
=fun pgm -> 
	_nl_run pgm []
 
end
(* 
open Problem4;;
open Problem5;;
let myPgm = (LET ("myFun", 
				   PROC("x", ADD( VAR "x", CONST 10)),
				   LET("x",
 				       CONST 7, 
					   CALL (VAR "myFun", VAR"x"))))
let myPgm2 = (CALL 
				(CALL 
					(PROC("x", 
						  PROC("y", 
							   SUB(VAR "x", VAR "y"))),
  				     CONST 5),
				 CONST 10))

let myNlPgm = translate myPgm ;;
let myNlPgm2 = translate myPgm2 ;;

nl_run myNlPgm ;;
nl_run myNlPgm2 ;; *)
 