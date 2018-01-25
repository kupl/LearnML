(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(***********************************)
(**            Problem 1          **)
(***********************************)
module Problem1= struct
type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec sumweight : branch -> int
 =fun b ->
 match b with
 SimpleBranch (l,w) -> w
 |CompoundBranch (l,m) -> 
 ( match m with
   (lb,rb) -> sumweight(lb)+sumweight(rb)
 )

let rec helper : branch -> int
 =fun b ->
 match b with 
 SimpleBranch (l,w) -> l*w
 | CompoundBranch (l,m)-> 
 ( match m with 
   |(lb,rb) -> 
   if(helper lb=(-1000001)) then -1000001
   else if(helper rb=(-1000001)) then -1000001
   else if(helper lb!= helper rb) then -1000001
   else l*(sumweight lb +sumweight rb)
 )

let balanced : mobile -> bool
 =fun (lb,rb) -> (* TODO *)
 if(helper lb=(-1000001)) then false
 else if(helper rb=(-1000001)) then false
 else if(helper lb=helper rb) then true else false
 
end





(***********************************)
(**            Problem 2          **)
(***********************************)
module Problem2=struct
type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let rec ifexist : var*string list-> bool
=fun (v,lst)->
match lst with
[] -> false
|h::t -> if(v=h) then true else ifexist(v,t)

let rec helpcheck : exp * string list -> bool
=fun (e,lst) ->
match e with
| V v-> ifexist(v,lst)
| P (v,ex) -> helpcheck(ex,v::lst)
| C (v1,v2) -> if(helpcheck(v1,lst)=true) then (if(helpcheck(v2,lst)=true) then true else false) else false

let check : exp -> bool
=fun e ->  (* TODO *)
helpcheck(e,[]) 
end
(***********************************)
(**            Problem 3          **)
(***********************************)

module Problem3=struct
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
let apply_env (x,e) = e x


let rec eval : program * env -> value
=fun (pgm,env) ->
match pgm with
CONST a -> Int a
|VAR v -> apply_env (v,env)
|ADD (ex1,ex2) -> 
let v1= eval (ex1,env) in
let v2= eval (ex2,env) in
(
  match v1,v2 with
  | Int n1, Int n2 -> Int (n1+n2)
  |_ -> raise (Failure "Environment is empty")
) 
|SUB (ex1,ex2) ->
let v1= eval (ex1,env) in
let v2= eval (ex2,env) in
(
  match v1,v2 with
  | Int n1, Int n2 -> Int (n1-n2)
  |_ -> raise (Failure "Environment is empty")
) 
|ISZERO (ex) -> 
(match eval(ex,env) with
  Int n -> if(n=0) then Bool true else Bool false
  |_-> raise (Failure "syntax error")
)
|IF (ex1,ex2,ex3) -> 
(match eval(ex1,env) with
  Bool b-> if(b=true) then eval(ex2,env) else eval(ex3,env)
  |_-> raise (Failure "syntax error")
)
|LET (v,ex1,ex2) -> 
  let v1 = eval (ex1,env) in
      eval (ex2,(extend_env (v,v1) env))
|LETREC (v1,v2,ex1,ex2) -> 
  eval (ex2,(extend_env (v1,RecProcedure (v1,v2,ex1,env)) env))
|PROC (v,ex) -> Procedure (v,ex,env)
|CALL (ex1,ex2) ->
  match eval(ex1,env) with
  RecProcedure (f,x,e,env2) -> 
  (
    let v=eval (ex2,env) in
    eval(e, extend_env (f,RecProcedure (f,x,e,env2)) (extend_env (x,v) env2))
  )
  |Procedure (x,e,env2)->
  (
    let v=eval (ex2,env) in
    eval(e, extend_env (x,v) env2)
  )
  |_ -> raise (Failure "Can't find") 

let rec run : program -> value
=fun pgm -> (* TODO *)
eval(pgm, empty_env)

end
(***********************************)
(**            Problem 4          **)
(***********************************)
module Problem4=struct
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

let rec checkstack : string list * string * int -> int
= fun (lst,v,cnt) ->
match lst with
[] -> raise (Failure "Environment is empty")
| h::t -> if(v=h) then cnt else checkstack(t,v,cnt+1)

let rec nl_translate : program * string list -> nl_program
= fun (pgm,lst) ->
match pgm with
CONST a -> NL_CONST a
| VAR v -> NL_VAR (checkstack(lst,v,0))
| ADD (ex1,ex2) -> NL_ADD(nl_translate(ex1,lst),nl_translate(ex2,lst))
| SUB (ex1,ex2) -> NL_SUB(nl_translate(ex1,lst),nl_translate(ex2,lst))
| ISZERO (ex) -> NL_ISZERO(nl_translate(ex,lst))
| IF (ex1,ex2,ex3) -> NL_IF(nl_translate(ex1,lst),nl_translate(ex2,lst),nl_translate(ex3,lst))
| LET (v,ex1,ex2) -> NL_LET(nl_translate(ex1,lst),nl_translate(ex2,v::lst))
| PROC (v,ex) -> NL_PROC(nl_translate(ex,v::lst))
| CALL (ex1,ex2) -> NL_CALL(nl_translate(ex1,lst),nl_translate(ex2,lst))

let translate : program -> nl_program
=fun pgm ->  (* TODO *)
nl_translate(pgm,[])
end
(***********************************)
(**            Problem 5          **)
(***********************************)
module Problem5=struct
open Problem4
type nl_value = NL_Int of int 
              | NL_Bool of bool 
              | NL_Procedure of nl_exp * nl_env
and nl_env = nl_value list

let rec find_var: int * nl_env -> nl_value
=fun (a,lst) ->
match lst with
[]-> raise (Failure "syntax error")
|h::t -> if(a=0) then h else find_var(a-1,t)

let rec nl_eval : nl_program * nl_env -> nl_value
=fun (pgm,env) ->
match pgm with
NL_CONST a-> NL_Int a
|NL_VAR a-> find_var(a,env)
|NL_ADD (ex1,ex2) ->
let v1= nl_eval (ex1,env) in
let v2= nl_eval (ex2,env) in
(
  match v1,v2 with
  | NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
  |_ -> raise (Failure "Environment is empty")
) 
|NL_SUB (ex1,ex2) ->
let v1= nl_eval (ex1,env) in
let v2= nl_eval (ex2,env) in
(
  match v1,v2 with
  | NL_Int n1, NL_Int n2 -> NL_Int (n1-n2)
  |_ -> raise (Failure "Environment is empty")
) 
|NL_ISZERO (ex) ->
(
  match nl_eval(ex,env) with
  NL_Int n -> if(n=0) then NL_Bool true else NL_Bool false
  |_-> raise (Failure "syntax error")
)
|NL_IF (ex1,ex2,ex3) ->
(
  match nl_eval(ex1,env) with
  NL_Bool b-> if(b=true) then nl_eval(ex2,env) else nl_eval(ex3,env)
  |_-> raise (Failure "syntax error")
)
|NL_LET (ex1,ex2) ->
(
  let v=nl_eval(ex1,env) in
  nl_eval(ex2,v::env)
)
|NL_PROC (ex) -> NL_Procedure(ex,env)
|NL_CALL (ex1,ex2) ->
match nl_eval(ex1,env) with
NL_Procedure(e,env2)-> let v=nl_eval(ex2,env) in nl_eval(e,v::env2)
|_ -> raise (Failure "Can't find") 

let nl_run : nl_program -> nl_value
=fun pgm -> (* TODO *)
nl_eval(pgm,[])
end