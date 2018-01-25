(* 1. You can modify the given function specifications as recursive. *) (* 2. However, do not modify the function names or types. *) (* 3. It is free to define any helper functions. *) (***********************************) (** Problem 1 **) (***********************************) module Problem1 = struct type mobile = branch * branch and branch = SimpleBranch of length * weight | CompoundBranch of length * mobile and length = int and weight = int let balanced : mobile -> bool =fun (lb,rb) -> false let rec wei p =
	match p with
	(SimpleBranch(a1,b1),CompoundBranch(a2,b2)) -> b1 + (wei b2)
	| (SimpleBranch(a1,b1), SimpleBranch(a2,b2)) -> b1 + b2
	| (CompoundBranch(a1,b1), SimpleBranch(a2,b2)) -> (wei b1) + b2
	| (CompoundBranch(a1,b1), CompoundBranch(a2,b2)) -> (wei b1) + (wei b2)

let rec balanced p = 
	match p with	
	(SimpleBranch(a1,b1),CompoundBranch(a2,b2)) -> if (a1 * b1) = (a2 * (wei b2)) then (balanced b2) else false
	| (SimpleBranch(a1,b1), SimpleBranch(a2,b2)) -> if (a1 * b1) = (a2 * b2) then true else false
	| (CompoundBranch(a1,b1), SimpleBranch(a2,b2)) -> if (a1 * (wei b1)) = (a2  * b2) then (balanced b1) else false
	| (CompoundBranch(a1,b1), CompoundBranch(a2,b2)) -> if (a1 * (wei b1)) = (a2 * (wei b2)) then (balanced b1) && (balanced b2) else false

end

(***********************************) (** Problem 2 **) (***********************************) module Problem2 = struct type exp = V of var | P of var * exp | C of exp * exp and var = string let check : exp -> bool =fun e -> true 
let rec evals a t = 
	match t with
	[] -> false
	| hd::tl -> if hd = a then true else evals a tl

let rec div a t =
	match a with
	V a -> evals a t
	| P (a,b) -> div b (a::t)
	| C (a,b) -> (div a t) && (div b t)

let chk a = div a []
end

 (***********************************) (** Problem 3 **) (***********************************) module Problem3 = struct type program = exp and exp = | CONST of int | VAR of var | ADD of exp * exp | SUB of exp * exp | ISZERO of exp | IF of exp * exp * exp | LET of var * exp * exp | LETREC of var * var * exp * exp | PROC of var * exp | CALL of exp * exp and var = string type value = Int of int | Bool of bool | Procedure of var * exp * env | RecProcedure of var * var * exp * env and env = var -> value let empty_env = fun _ -> raise (Failure "Environment is empty") let extend_env (x,v) e = fun y -> if x = y then v else (e y) let apply_env e x = e x let run : program -> value =fun pgm -> Int 0 
 let cal p =
	match p with
	Int a -> a

let rec eval p ev = 
	match p with
	CONST a -> Int a
	| VAR a -> apply_env ev a
	| ADD(a,b) -> Int (cal (eval a ev) + cal (eval b ev))
	| SUB(a,b) -> Int (cal (eval a ev) - cal (eval b ev))
	| ISZERO a -> if (eval a ev) = Int 0 then Bool true else Bool false
	| IF (a,b,c) -> if (eval a ev) = Bool true then (eval b ev) else (eval c ev)
	| LET (a,b,c) -> eval c (extend_env (a , eval b ev) ev)
	| LETREC(a,b,c,d) -> eval d (extend_env (a, RecProcedure(a,b,c,ev)) ev)
	| PROC (a,b) -> Procedure(a,b,ev)
	| CALL (a1,b1) -> match (eval a1 ev) with
	Procedure(a,b,c) -> eval (LET(a,b1,b)) ev
	| RecProcedure(a,b,c,d) -> eval (LET(b,b1,c)) ev

let run a = eval a empty_env
end 

(***********************************) (** Problem 4 **) (***********************************) module Problem4 = struct type program = exp and exp = | CONST of int | VAR of var | ADD of exp * exp | SUB of exp * exp | ISZERO of exp | IF of exp * exp * exp | LET of var * exp * exp | PROC of var * exp | CALL of exp * exp and var = string type nl_program = nl_exp and nl_exp = | NL_CONST of int | NL_VAR of int | NL_ADD of nl_exp * nl_exp | NL_SUB of nl_exp * nl_exp | NL_ISZERO of nl_exp | NL_IF of nl_exp * nl_exp * nl_exp | NL_LET of nl_exp * nl_exp | NL_PROC of nl_exp | NL_CALL of nl_exp * nl_exp let translate : program -> nl_program =fun pgm -> NL_CONST 0
let rec count a =
	match a with
	[] -> 0
	| hd::tl -> 1 + count tl

let rec find a b =
	match a with
	[] -> 0
	| hd::tl -> if hd = b then (count a)-1 else find tl b


let rec evol p ev =
	match p with
	CONST a -> NL_CONST a
	| VAR a -> NL_VAR (find ev a)
	| ADD (a,b) -> NL_ADD (evol a ev , evol b ev )
	| SUB (a,b) -> NL_SUB (evol a ev , evol b ev )
	| ISZERO a -> NL_ISZERO (evol a ev )
	| IF (a,b,c) -> NL_IF (evol a ev , evol b ev , evol c ev )
	| LET (a,b,c) -> NL_LET(evol b (ev) , evol c (ev@[a]) )
	| PROC (a,b) -> NL_PROC(evol b (ev@[a]))
	| CALL (a,b) -> NL_CALL(evol a ev , evol b ev )

let translate a = evol a []
end

(***********************************) (** Problem 5 **) (***********************************) module Problem5 = struct open Problem4 type nl_value = NL_Int of int | NL_Bool of bool | NL_Procedure of nl_exp * nl_env and nl_env = nl_value list let nl_run : nl_program -> nl_value =fun pgm -> NL_Int 0
let rec count a =
	match a with
	[] -> 0
	| hd::tl -> 1 + count tl

let rec find ev a =
	match ev with
	[] -> raise(Failure"Enviroment is empty")
	| hd::tl -> if (count ev) - a = 1 then hd else
	find tl a

let cal a = match a with
	NL_Int a -> a
	| NL_Bool _ -> 1
	| NL_Procedure _ -> 1

let rec evil p ev =
	match p with
	NL_CONST a -> NL_Int a
	| NL_VAR a -> (find ev a)
	| NL_ADD (a,b) -> NL_Int (cal (evil a ev) + cal (evil b ev))
	| NL_SUB (a,b) -> NL_Int (cal (evil a ev) - cal (evil b ev))
	| NL_ISZERO a -> if (evil a ev) = NL_Int 0 then NL_Bool true else NL_Bool false
	| NL_IF (a,b,c) -> if (evil a ev) = NL_Bool true then (evil b ev) else (evil c ev)
	| NL_LET (a,b) -> evil b (ev@[(evil a ev)])
	| NL_PROC a -> NL_Procedure(a,ev)
	| NL_CALL (a,b) -> match (evil a ev) with
	NL_Procedure(a,b) -> (evil a (ev@[NL_Procedure(a,b)]))

let run a =	evil a []
end