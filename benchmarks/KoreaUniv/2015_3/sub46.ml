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
  and weight = int;;

  let rec weigh : mobile -> int
  =fun (left,right) -> match left with
    | SimpleBranch (_,lw) -> (match right with
        | SimpleBranch (_,rw) -> lw + rw
        | CompoundBranch (_,rother) -> lw + weigh rother)
    | CompoundBranch (_,lother) -> (match right with
        | SimpleBranch (_,rw) -> (weigh lother) + rw
        | CompoundBranch (_,rother) -> (weigh lother) + (weigh rother));;

  let rec balanced : mobile -> bool
  =fun (left,right) -> match left with
    | SimpleBranch (left_length,left_weight) -> (match right with
        | SimpleBranch (right_length,right_weight) -> left_length*left_weight = right_length*right_weight
        | CompoundBranch (right_length,right_other) -> (balanced right_other)
                                                    && (left_length*left_weight = right_length*(weigh right_other)))
    | CompoundBranch (left_length,left_other) -> (match right with
        | SimpleBranch (right_length,right_weight) -> (balanced left_other)
                                                   && (left_length*(weigh left_other) = right_length*right_weight)
        | CompoundBranch (right_length,right_other) -> (balanced left_other)
                                                    && (balanced right_other)
                                                    && (left_length*(weigh left_other) = right_length*(weigh right_other)));;

(* Test 1 *)
(*
let test_case = (CompoundBranch (3,
                 (CompoundBranch (2, (SimpleBranch (1, 1), SimpleBranch (1, 1))),
                                  SimpleBranch (1, 4))),
                                  SimpleBranch (6, 3));;
if not (balanced test_case) then print_string("Fail test 1");;
*)
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string;;

  module SS = Set.Make(String);;

  let rec echeck : exp*SS.t -> bool
  =fun (e, vars) -> match e with
    | V variable -> not (SS.is_empty (SS.inter (SS.singleton variable) vars))
    | P (variable,expression) -> echeck (expression,SS.add variable vars)
    | C (expression1,expression2) -> echeck (expression1,vars) && echeck(expression2,vars);;

  let check : exp -> bool
  =fun e -> echeck (e,SS.empty);;

(* Test cases *)
(*
    let test1 = P ("a", V "a");;
    let test2 = P ("a", P ("a", V "a"));;
    let test3 = P ("a", P ("b", C (V "a", V "b")));;
    let test4 = P ("a", C (V "a", P ("b", V "a")));;
    if not (check test1 && check test2 && check test3 && check test4)
    then print_string("Fail 2.1");;

    let test1 = P ("a", V "b");;
    let test2 = P ("a", C (V "a", P ("b", V "c")));;
    let test3 = P ("a", P ("b", C (V "a", V "c")));;
    if (check test1 || check test2 || check test3)
    then print_string("Fail 2.2");;
*)
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
  and var = string;;

  type value = Int of int | Bool of bool 
             | Procedure of var * exp * env 
             | RecProcedure of var * var * exp * env
  and env = var -> value;;
  
  let empty_env = fun _ -> raise (Failure "Environment is empty");;
  let extend_env (x,v) e = fun y -> if x = y then v else (e y);;
  let apply_env e x = e x;;

  let rec eval_bop : (int -> int -> int) -> exp -> exp -> env -> value
  =fun op e1 e2 env ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
        (match v1,v2 with
        | Int n1, Int n2 -> Int (op n1 n2)
        | _ -> raise (Failure "Type Error: non-numeric values"))

  and eval : exp -> env -> value
  =fun exp env ->
    match exp with
    | CONST n -> Int n
    | VAR x -> apply_env env x
    | ADD (e1,e2) -> eval_bop (+) e1 e2 env
    | SUB (e1,e2) -> eval_bop (-) e1 e2 env
    | ISZERO e ->
            (let v = eval e env in
            match v with
      | Int n -> if n = 0 then Bool true else Bool false
      | _ -> raise (Failure "Type Error: subexpression of zero? must be Int type"))
    | IF (e1,e2,e3) ->
      (match eval e1 env with
        | Bool true -> eval e2 env
        | Bool false -> eval e3 env
        | _ -> raise (Failure "Type Error: condition must be Bool type"))
    | LET (x,e1,e2) ->
        let v = eval e1 env in
            eval e2 (extend_env (x,v) env)
    | LETREC (f,x,e1,e2) ->
        eval e2 (extend_env (x,RecProcedure (f,x,e1,env)) env)
    | PROC (x,e) -> Procedure (x,e,env)
    | CALL (e1,e2) ->
        (match eval e1 env with
        | Procedure (x,e,p) ->
            (let v = eval e2 env in
                eval e (extend_env (x,v) p))
        | RecProcedure (f,x,e,p) -> 
            (let v = eval e2 env in
                eval e (extend_env (x,v) (extend_env (f,RecProcedure (f,x,e,p)) p)))
        | _ -> raise (Failure "Type Error: must call a Procedure"));;
 
  let run : program -> value
  =fun pgm -> eval pgm empty_env;;

(* Tests *)
  (*
    let pgm1 = CONST 1;;
    let pgm2 = ADD (ADD (CONST 5, CONST 2), SUB (CONST 3, CONST 10));; (* (5+2)+(3-10) = 0 *)
    let pgm3 = IF (ISZERO (CONST 1), CONST 3, CONST 4);; (* if iszero 1 then 3 else 4 *)
    if not (run pgm1 = Int 1) then print_string("Fail 3.1");;
    if not (run pgm2 = Int 0) then print_string("Fail 3.2");;
    if not (run pgm3 = Int 4) then print_string("Fail 3.3");;
*)
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
(*
  let rec index
  =fun (l,e,agg) -> match l with
    | [] -> agg
    | h::t -> if h = e then agg+1 else index(t,e,agg+1);;

  let empty_env = [];;
  let extend_env e x = match index(e,x,-1) with
    | -1 -> x::e
    | _ -> raise (Failure "namespace collision.");;
  let apply_env e x = index(e,x,-1);;

  let rec trans : program * (string list) -> nl_program
  =fun (prog,env) ->
        match prog with
        | CONST n -> NL_CONST n
        | VAR x -> NL_VAR apply_env env x
        | ADD (e1,e2) ->
            NL_ADD (trans(e1,env),
                    trans(e2,env))
        | SUB (e1,e2) ->
            NL_SUB (trans(e1,env),
                    trans(e2,env))
        | ISZERO e -> NL_ISZERO trans(e,env)
        | IF (e1,e2,e3) ->
            NL_IF (trans(e1,env),
                   trans(e2,env),
                   trans(e3,env))
        | LET (x,e1,e2) ->
            let nl2 = trans(e1,env) in
            let nl1 = trans(e2,(extend_env env x)) in
            NL_LET (nl1, nl2)
        | PROC (_,e) -> NL_PROC (trans(e,env))
        | CALL (e1,e2) ->
            (match trans(e1,env) with
            | NL_PROC (x,e,p) ->
                (let nl2 = trans(e2,env) in
                    trans(e,(extend_env p x))))
*)
  let rec translate : program -> nl_program
  =fun pgm -> NL_CONST 0;; (*
    let count = ref 0 in
    let next_count () = (count := !count + 1; !count - 1) in
    let nl_env = empty_env in
    trans (pgm,nl_env,next_count);;
  *)

(* Tests *)
(*
  let pgm0 = ADD (VAR "y", VAR "x");;
  let test0 = translate(pgm0);;
  let pgm1 = LET ("x", CONST 37,
    PROC ("y", LET ("z", ADD (VAR "y", VAR "x"),
        ADD (VAR "x", VAR "y"))));;
  let test1 = translate(pgm1);;
  *)
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
  (*
    match pgm with
    | NL_CONST n -> NL_Int n
    | NL_VAR n -> nl_env
    | NL_ADD of nl_exp * nl_exp
    | NL_SUB of nl_exp * nl_exp
    | NL_ISZERO of nl_exp
    | NL_IF of nl_exp * nl_exp * nl_exp
    | NL_LET of nl_exp * nl_exp
    | NL_PROC of nl_exp 
    | NL_CALL of nl_exp * nl_exp
    *)
 
  let nl_run : nl_program -> nl_value
  =fun pgm -> NL_Int 0;; (** eval pgm empty_env;; *)
end
