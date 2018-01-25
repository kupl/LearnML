(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(***********************************)
(**            Problem 1          **)
(***********************************)

type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec calculate : mobile -> weight
 = fun (lb, rb) ->
  match lb, rb with
  | SimpleBranch(l1,w1), SimpleBranch(l2,w2) -> w1+w2
  | SimpleBranch(l1,w1), CompoundBranch(l2,m) -> 
    ( match m with 
      | l, r -> w1 + calculate(l,r) 
    )
  | CompoundBranch(l1,m), SimpleBranch(l2,w2) -> 
    ( match m with 
      | l, r -> w2 + calculate(l,r) 
    )
  | CompoundBranch(l1,m1), CompoundBranch(l2,m2) -> 
    ( match m1 with 
      | l, r -> 
        (match m2 with
          | ll, rr -> calculate(l,r) + calculate(ll,rr)
        )
    )

let balanced : mobile -> bool
=fun (lb,rb) ->
  match lb, rb with
  | SimpleBranch(l1,w1), SimpleBranch(l2,w2) ->
    if l1*w1 = l2*w2 then true 
    else false
  | SimpleBranch(l1,w1), CompoundBranch(l2,m) ->
    ( match m with 
      | l, r -> if l1*w1 = l2*calculate(l,r) then true
    else false
    )
  | CompoundBranch(l1,m), SimpleBranch(l2,w2) ->
    ( match m with 
      | l, r -> if l2*w2 = l1*calculate(l,r) then true
    else false
    )
  | CompoundBranch(l1,m1), CompoundBranch(l2,m2) ->
    ( match m1 with 
      | l, r ->
        (match m2 with
          | ll, rr -> if l1*calculate(l,r) = l2*calculate(ll,rr) then true
            else false
        )
    )

(***********************************)
(**            Problem 2          **)
(***********************************)

type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let rec chars : exp -> var
= fun e ->
  match e with
  | V a -> a
  | P(v,e1) ->
    (match e1 with
    | V b -> b
    | P (b, e2) -> chars e2
    | C (e1, e2) -> chars e2
    )
  | C(e1,e2) ->
    ( match e1 ,e2 with
    | V b, V c -> c
    | V b, P(c, e3) -> chars e3
    )


let check : exp -> bool
=fun e ->
  match e with
  | V a -> true
  | P(v,e1) ->
    (match e1 with
    | V b -> if v = b then true else false
    | P (b, e2) -> if (chars e2) = v || chars e2 = b then true else false
    | C (e1, e2) -> if chars e1 = v || chars e2 = v then true else false
    )
  | C(e1, e2)->
    (match e1 ,e2 with
    | V b, V c -> true
    | V b, P(c, e3) -> if c = b || b = chars e3 then true else false
    )


(***********************************)
(**            Problem 3          **)
(***********************************)
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

let run : program -> value
=fun pgm -> Int 0 (* TODO *)

(***********************************)
(**            Problem 4          **)
(***********************************)

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

(***********************************)
(**            Problem 5          **)
(***********************************)

type nl_value = NL_Int of int 
              | NL_Bool of bool 
              | NL_Procedure of nl_exp * nl_env
and nl_env = nl_value list

let nl_run : nl_program -> nl_value
=fun pgm -> NL_Int 0 (* TODO *)
