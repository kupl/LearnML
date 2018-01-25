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
  =fun (x1,x2) -> 





let rec helper (b1,b2) =
match b1,b2 with
SimpleBranch(l1,w1), SimpleBranch(l2,w2)-> w1 + w2

|CompoundBranch(l1,(subbran1,subbran2)),SimpleBranch(l2,w2)->
helper((subbran1,subbran2)) + w2

|SimpleBranch(l2,w2),CompoundBranch(l1,(subbran1,subbran2))->
helper((subbran1,subbran2)) + w2

|CompoundBranch(l1,(subbran1,subbran2)),CompoundBranch(l2,(subbran3,subbran4))->
helper((subbran1,subbran2)) + helper((subbran3,subbran4))in

match x1,x2 with
SimpleBranch(l1,w1), SimpleBranch(l2,w2) -> if((l1*w1)==(l2*w2)) then true else false
|SimpleBranch(l1,w1), CompoundBranch(l2,(a,b))->if((l1*w1)==(l2*(helper(a,b)))) then true else false
|CompoundBranch(l1,(a,b)),SimpleBranch(l2,w2)->if((l1*(helper(a,b)))==l2*w2) then true else false
|CompoundBranch(l1,(a1,b1)),CompoundBranch(l2,(a2,b2))->if((l1*(helper(a1,b1)))==(l2*(helper(a2,b2)))) then true else false;;



end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  


let check a =

   let rec isinlst l x =
   match l with
   [] -> false
   |hd::tl -> if (hd = x) then true else isinlst tl x in

   let rec letsdoit e lst =
   match e with
   V i -> isinlst lst i
   |P(i,j) -> letsdoit j (i :: lst)
   |C(i,j) -> (letsdoit i lst)&&(letsdoit j lst)
 in letsdoit a [];;






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
