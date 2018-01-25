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
= fun b ->
match b with
|SimpleBranch (l,w) -> w
|CompoundBranch (l,(a,b)) -> findweight a + findweight b

let rec balanced : mobile -> bool
  =fun (lb,rb) -> 
match lb with
|CompoundBranch (l,(a,b))->
	if balanced (a,b) then
	(match rb with
	|CompoundBranch (m,(c,d))-> if balanced (c,d) then
					((l*(findweight lb)) = (m*(findweight rb))) else false
	|SimpleBranch (m,x)-> ((l*findweight lb)=(m*x))
	) else false
|SimpleBranch (l,w)->
	(match rb with
	|CompoundBranch (m,(c,d))-> if balanced (c,d) then
					((l*w) = (m*(findweight rb))) else false
	|SimpleBranch (m,x)-> ((l*w)=(m*x))
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
  =fun e -> match e with
|P(v,e)-> (match e with
		|V a -> v=a
		|P(a,b) -> check e
		|C(a,b) -> (check a) && (check b)
	  )
|C(a,b)-> (check a) && (check b)
|V a -> true
|_ -> false

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

let rec calc : program -> env -> value
=fun pgm e ->
match pgm with
|CONST i-> Int i
| VAR x-> apply_env e x
| ADD (a,b)-> (match calc a e,calc b e with
		|Int c,Int d-> Int(c + d)
		|_->raise (Failure "Type Error"))
| SUB (a,b)-> (match calc a e, calc b e with
		|Int c,Int d-> Int(c - d)
		|_->raise (Failure "Type Error"))
| ISZERO a-> if ((calc a e)=Int 0) then Bool true else Bool false
| IF (a,b,c)-> if (calc a e)=Bool true then (calc b e) else (calc c e)
| LET (x,a,b)-> calc b (extend_env (x,calc a e) e)
| LETREC (f,x,a,b)-> calc b (extend_env ( f, RecProcedure(f,x,a,e) ) e)
| PROC (x,a)-> Procedure(x,a,e)
| CALL (e1,e2)->
	(match calc e1 e with
	|Procedure(x,a,e0)-> calc a (extend_env (x,(calc e2 e)) e0)	
	|RecProcedure(f,x,a,e0)-> 
   calc a ( extend_env (x,(calc e2 e)) (extend_env ( f,RecProcedure(f,x,a,e0) ) e0) )
	|_->raise (Failure "Type Error")
	)
 
  let run : program -> value
  =fun pgm -> calc pgm empty_env

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

  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list

let rec nth l n=
match l with
|[]-> raise (Failure "Empty Environment")
|h::t-> if n=0 then h else (nth t (n-1))

let rec find l a=
match l with
|[]-> raise (Failure "Not Found")
|h::t-> if h=a then 0 else (1+(find t a))

let rec trans : program -> 'a list -> nl_program
=fun pgm l ->
match pgm with
| CONST i-> NL_CONST i
| VAR x-> NL_VAR (find l x)
| ADD (a,b)-> NL_ADD(trans a l, trans b l)
| SUB (a,b)-> NL_SUB(trans a l, trans b l)
| ISZERO a-> NL_ISZERO (trans a l)
| IF (a,b,c)-> NL_IF ((trans a l),(trans b l),(trans c l))
| LET (x,a,b)-> NL_LET(trans a l, trans b (x::l))
| PROC (x,a)-> NL_PROC(trans a (x::l))
| CALL (a,b)-> NL_CALL((trans a l), (trans b l))


  let translate : program -> nl_program
  =fun pgm -> trans pgm []

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

  
let rec ncalc : nl_program -> nl_env -> nl_value
=fun pgm l ->
match pgm with
|NL_CONST i-> NL_Int i
|NL_VAR i-> nth l i
|NL_ADD (a,b)-> (match ncalc a l,ncalc b l with
		|NL_Int c, NL_Int d-> NL_Int(c + d)
		|_->raise (Failure "Type Error"))
|NL_SUB (a,b)-> (match ncalc a l, ncalc b l with
		|NL_Int c, NL_Int d-> NL_Int(c - d)
		|_->raise (Failure "Type Error"))
|NL_ISZERO a-> if ((ncalc a l)=NL_Int 0) then NL_Bool true else NL_Bool false
|NL_IF (a,b,c)-> if (ncalc a l)=NL_Bool true then (ncalc b l) else (ncalc c l)
|NL_LET (a,b)-> ncalc b ((ncalc a l)::l)
|NL_PROC a-> NL_Procedure(a,l)
|NL_CALL (e1,e2)->
	(match ncalc e1 l with
	|NL_Procedure(a,l0)-> ncalc a ((ncalc e2 l)::l0)
	|_->raise (Failure "Type Error")	
	)
	
  let nl_run : nl_program -> nl_value
  =fun pgm -> ncalc pgm []

end
