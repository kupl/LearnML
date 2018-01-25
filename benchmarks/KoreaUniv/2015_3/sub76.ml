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
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) -> 
    let rec sum_weight m =
      match m with l,r ->
        match l,r with
        | SimpleBranch (_,w1), SimpleBranch (_,w2) -> w1 + w2
        | CompoundBranch (_,mo1), SimpleBranch (_,w1) -> sum_weight mo1 + w1
        | CompoundBranch (_,mo1), CompoundBranch (_,mo2)  -> sum_weight mo1 + sum_weight mo2
        | SimpleBranch (_,w1), CompoundBranch (_,mo1)  -> w1 + sum_weight mo1
    in

    match lb,rb with
    | SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> if l1*w1 == l2*w2 then true else false
    | CompoundBranch (l1,mo1), SimpleBranch (l2,w1) -> if balanced mo1 && (l1*sum_weight mo1 == l2*w1) then true else false
    | CompoundBranch (l1,mo1), CompoundBranch (l2,mo2)  -> if balanced mo1 && balanced mo2 && (l1*sum_weight mo1 == l2*sum_weight mo2) then true else false
    | SimpleBranch (l1,w1), CompoundBranch (l2,mo1)  -> if balanced mo1 && (l2*sum_weight mo1 == l1*w1) then true else false
end

(***********************************)
(**            Problem 2          **)
(***********************************)
(*do it later*)
module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec check : exp -> bool
  =fun e -> 
    let rec remove_var exp var =
    match exp with
    | P (v,ex) -> P (v, remove_var ex var)
    | C (ex,ex') -> C (remove_var ex var, remove_var ex' var)
    | V v -> if (v = var) then V "" else V v
    in

    match e with
    | P (v,ex) -> check (remove_var ex v)
    | C (ex,ex') -> check ex && check ex'
    | V v -> if v<> "" then false else true
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
  
  let rec translate : program -> nl_program
  =fun pgm ->
     let rec num_var : exp -> var -> int -> exp = fun exp v n ->
     match exp with
     | CONST i -> CONST i
     | VAR v' -> if v' = v then VAR (string_of_int n) else VAR v'
     | ADD (x,y) -> ADD (num_var x v n, num_var y v n)
     | SUB (x,y) -> SUB (num_var x v n, num_var y v n)
     | ISZERO x -> ISZERO (num_var x v n)
     | IF (x,y,z) -> IF (num_var x v n, num_var y v n, num_var z v n)
     | LET (v',x,y) -> LET (v', (num_var x v n), (num_var y v (n+1)))
     | PROC (v',x) -> PROC (v', (num_var x v (n+1)))
     | CALL (x,y) -> CALL ((num_var x v (n+1)), (num_var y v (n+1)))
   in

    match pgm with
     | LET (v,x,y) -> NL_LET (translate x, translate (num_var y v 0))
     | PROC (v,x) -> NL_PROC (translate (num_var x v 0))
     | CONST i -> NL_CONST i
     | VAR v -> NL_VAR (int_of_string v)
     | ADD (x,y) -> NL_ADD (translate x, translate y)
     | SUB (x,y) -> NL_SUB (translate x, translate y)
     | ISZERO x -> NL_ISZERO (translate x)
     | IF (x,y,z) -> NL_IF (translate x, translate y , translate z)
     | CALL (x,y) -> NL_CALL (translate x, translate y)
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
