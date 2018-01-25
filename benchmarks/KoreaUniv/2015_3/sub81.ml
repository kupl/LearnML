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
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> false (* TODO *)
end

open Problem1;;
open Printf;;

let rec helper (b1,b2) =
match b1,b2 with
SimpleBranch(l1,w1),SimpleBranch(l2,w2) -> if (l1*w1=l2*w2) then w1+w2 else -10000
| CompoundBranch(l1,(sub1,sub2)),SimpleBranch(l2,w2) -> helper(sub1,sub2) + w2
| SimpleBranch(l1,w1),CompoundBranch(l2,(sub1,sub2)) -> w1 + helper(sub1,sub2)
| CompoundBranch(l1,(sub1,sub2)),CompoundBranch(l2,(sub3,sub4)) ->
                                                        helper(sub1,sub2) + helper(sub3,sub4);;

let rec balanced (b1,b2)=
match b1,b2 with
SimpleBranch(l1,w1),SimpleBranch(l2,w2) -> if(l1*w1) = (l2*w2) then true else false
| SimpleBranch(l1,w1),CompoundBranch(l2,(s1,s2)) ->
                if(l2*(helper(s1,s2))) = (l1*w1) then true else false
| CompoundBranch(l1,(s1,s2)),SimpleBranch(l2,w2) ->
                if(l2*w2) = (l1*(helper(s1,s2))) then true else false
| CompoundBranch(l1,(s1,s2)),CompoundBranch(l2,(s3,s4)) ->
                if(l2*(helper(s3,s4))) = (l1*(helper(s1,s2))) then true else false;;

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let check : exp -> bool
  =fun e -> true (* TODO *)
end

open Problem2;;
open List;;


let check input =
let rec helpCheck a lst =
match a with
V(a) -> exists (fun a->a=a) lst
|P(a,b) -> helpCheck b (a::lst)
|C(a,b) -> helpCheck a lst && helpCheck b lst in
helpCheck input [];;



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
  
  let run : program -> value
  =fun pgm -> Int 0 (* TODO *)
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
