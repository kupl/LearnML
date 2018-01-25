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


  let rec getWeight : branch -> weight
    =fun br ->
      match br with
          SimpleBranch(l, w) -> w
        | CompoundBranch(l, m) -> 
            match m with
                (br1, br2) -> getWeight(br1) + getWeight(br2)

  let rec calBranch : branch -> int
    =fun br ->
      match br with
          SimpleBranch (l, w) -> l * w
        | CompoundBranch (l, m) ->
            match m with
                (br1, br2) ->
                  if calBranch(br1) = calBranch(br2)
                  then l * (getWeight(br1) + getWeight(br2))
                  else -1

  let balanced : mobile -> bool
    =fun (lb,rb) ->
      if calBranch(lb) < 0 || calBranch(rb) < 0 || calBranch(lb) != calBranch(rb)
      then false
      else true

end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let rec pcheck : var list * exp -> bool
    =fun (var, exp) ->
      match exp with
          V evar -> List.exists(fun v -> v = evar) var
        | P (v, exp) -> pcheck(v::var, exp)
        | C (e1, e2) -> pcheck(var, e1) && pcheck(var ,e2)

  let check : exp -> bool
    =fun e ->
      match e with
          V v-> false
        | P (v, exp) -> pcheck(v::[], exp)
        | C (e1, e2) -> false
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


  let rec eval_bop : (int -> int -> int) -> exp -> exp -> env -> value
    =fun op e1 e2 env ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
        (match v1,v2 with
            Int n1, Int n2 -> Int (op n1 n2)
          | _ -> raise (Failure "Type Error: non-numeric values")
        )

  and eval : exp -> env -> value
    =fun exp env ->
      match exp with
          CONST n -> Int n

        | VAR x -> apply_env env x
        | ADD (e1,e2) -> eval_bop (+) e1 e2 env
        | SUB (e1,e2) -> eval_bop (-) e1 e2 env
        | ISZERO e ->
            let v = eval e env in
              (match v with
                  Int n -> if n = 0 then Bool true else Bool false
                | _ -> raise (Failure "Type Error: subexpression of zero? must be Int type")
              )
        | IF (e1,e2,e3) ->
            (match eval e1 env with
              | Bool true -> eval e2 env
              | Bool false -> eval e3 env
              | _ -> raise (Failure "Type Error: condition must be Bool type")
            )
        | LET (x,e1,e2) ->
            let v1 = eval e1 env in
              eval e2 (extend_env (x, v1) env)
        | LETREC (f, x, e1, e2) ->
            let v1 = RecProcedure (f, x, e1, env) in
              eval e2 (extend_env (f, v1) env)
        | PROC (x, e) ->
            Procedure (x, e, env)
        | CALL (e1, e2) ->
            let v1 = eval e1 env in
            let v2 = eval e2 env in
              (match v1 with
                  Procedure (x, e, env2) -> eval e (extend_env (x, v2) env2)
                | RecProcedure (f, x, e, env2) -> eval e (extend_env (f, v1) (extend_env (x, v2) env))
                | _ -> raise (Failure "Type Error")
              )


  let run : program -> value
    =fun pgm -> eval pgm empty_env
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

  let varList = []

  let rec popVar : var -> var list -> int
    =fun v l ->
      match l with
          [] -> raise (Failure "empty stack")
        | hd::tl -> if v = hd then 0 else (popVar v tl) + 1

  let rec _trans : exp -> var list -> nl_program
    =fun e l ->
      match e with
          CONST n -> NL_CONST n
        | VAR x -> NL_VAR (popVar x l)
        | ADD (e1, e2) -> NL_ADD (_trans e1 l, _trans e2 l)
        | SUB (e1, e2) -> NL_SUB (_trans e1 l, _trans e2 l)
        | ISZERO e -> NL_ISZERO (_trans e l)
        | IF (e1, e2, e3) -> NL_IF (_trans e1 l, _trans e2 l, _trans e3 l)
        | LET (x, e1, e2) -> NL_LET(_trans e1 l, _trans e2 (x::l))
        | PROC (x, e) -> NL_PROC (_trans e (x::l))
        | CALL (e1, e2) -> NL_CALL (_trans e1 l, _trans e2 l)

  let rec translate : program -> nl_program
    =fun pgm -> _trans pgm varList

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

  let empty_env = []

  let rec getVar : int -> nl_env -> nl_value
    =fun i e ->
      (match e with
          [] -> raise (Failure "empty stack")
        | hd::tl -> if i = 0 then hd else (getVar (i - 1) tl)
      )

  let rec nl_eval_bop : (int -> int -> int) -> nl_program -> nl_program -> nl_env -> nl_value
    =fun op e1 e2 env ->
      let v1 = nl_eval e1 env in
      let v2 = nl_eval e2 env in
        (match v1,v2 with
            NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
          | _ -> raise (Failure "Type Error: non-numeric values")
        )

  and nl_eval : nl_program -> nl_env -> nl_value
    =fun pgm env ->
      (match pgm with
          NL_CONST n -> NL_Int n
        | NL_VAR x -> getVar x env
        | NL_ADD (e1, e2) -> nl_eval_bop (+) e1 e2 env
        | NL_SUB (e1, e2) -> nl_eval_bop (-) e1 e2 env
        | NL_ISZERO (e) ->
            (match nl_eval e env with
                NL_Int n -> if n = 0 then NL_Bool true else NL_Bool false
              | _ -> raise (Failure "Type error: subexpression of zero? must be NL_Int type")
            )
        | NL_IF(e1, e2, e3) ->
            (match nl_eval e1 env with
                NL_Bool true -> nl_eval e2 env
              | NL_Bool false -> nl_eval e3 env
              | _ -> raise (Failure "Type Error: condition must be Bool type")
            )
        | NL_LET (e1, e2) ->
            let v1 = nl_eval e1 env in
              nl_eval e2 (v1::env)
        | NL_PROC e ->
            NL_Procedure (e, env)
        | NL_CALL (e1, e2) ->
            let v1 = nl_eval e1 env in
            let v2 = nl_eval e2 env in
              (match v1 with
                  NL_Procedure (e, env2) -> nl_eval e (v2::env2)
                | _ -> raise (Failure "Type error")
              )
      )

  let nl_run : nl_program -> nl_value
    =fun pgm -> nl_eval pgm []
end

(*
  (* Test for Problem 1 *)

  let p1_t1 = Problem1.(SimpleBranch(1,1), SimpleBranch(1, 1)) in
  Problem1.balanced(p1_t1);;

  let p1_t2 = Problem1.(SimpleBranch(1,1), SimpleBranch(1, 2)) in
  Problem1.balanced(p1_t2);;

  let p1_t3 = Problem1.(CompoundBranch (3,(CompoundBranch (2, (SimpleBranch (1, 1), SimpleBranch (1, 1))),SimpleBranch (1, 4))),SimpleBranch (6, 3)) in
  Problem1.balanced(p1_t3);;

  let p1_t4 = Problem1.(CompoundBranch (2,(CompoundBranch (2, (SimpleBranch (1, 2), SimpleBranch (1, 2))),SimpleBranch (1, 5))),SimpleBranch (6, 3)) in
  Problem1.balanced(p1_t4);;

  (* Test for Problem 2 *)
  let p2_t1 = Problem2.(P ("a", V "a")) in
  Problem2.check(p2_t1);;

  let p2_t2 = Problem2.(P ("a", P ("a", V "a"))) in
  Problem2.check(p2_t2);;

  let p2_t3 = Problem2.(P ("a", P ("b", C (V "a", V "b")))) in
  Problem2.check(p2_t3);;

  let p2_t4 = Problem2.(P ("a", C (V "a", P ("b", V "a")))) in
  Problem2.check(p2_t4);;

  let p2_t5 = Problem2.(P ("a", V "b")) in
  Problem2.check(p2_t5);;

  let p2_t5 = Problem2.(P ("a", C (V "a", P ("b", V "c")))) in
  Problem2.check(p2_t5);;

  let p2_t5 = Problem2.(P ("a", P ("b", C (V "a", V "c")))) in
  Problem2.check(p2_t5);;

  (* Test for Problem 3  *)
  let p3_t1 = Problem3.(CONST 1) in
  Problem3.run(p3_t1);;

  let p3_t2 = Problem3.(ADD (ADD (CONST 5, CONST 2), SUB (CONST 3, CONST 10))) in
  Problem3.run(p3_t2);;

  let p3_t3 = Problem3.(IF (ISZERO (CONST 1), CONST 3, CONST 4)) in
  Problem3.run(p3_t3);;

  let p3_t4 = Problem3.(LET ("x", CONST 5, SUB(VAR "x", CONST 3))) in
  Problem3.run(p3_t4);;

  let p3_t5 = Problem3.(LET ("x", CONST 7, LET ("y", CONST 2, LET ("y", LET ("x", SUB(VAR "x", CONST 1),  SUB (VAR "x", VAR "y")), SUB (SUB (VAR "x", CONST 8), VAR "y"))))) in
  Problem3.run(p3_t5);;

  let p3_t6 = Problem3.(LET ("x", CONST 1, LET ("y", CONST 2, ADD (VAR "x", VAR "y")))) in
  Problem3.run(p3_t6);;

  (*
  let p3_t7 = Problem3.(ISZERO (ISZERO (CONST 1))) in
  Problem3.run(p3_t7);;
  *)

  let p3_t8 = Problem3.(LETREC ("double", "x", IF (ISZERO (VAR "x"), CONST 0, ADD (CALL (VAR "double", SUB (VAR "x", CONST 1)), CONST 2)), CALL (VAR "double", CONST 6))) in
  Problem3.run(p3_t8);;

  (* Test for Problem 4 *)
  let p4_t1 = Problem4.(CONST 10) in
  Problem4.translate(p4_t1);;

  let p4_t2 = Problem4.(LET ("x", CONST 37, PROC ("y", LET ("z", SUB (VAR "y", VAR "x"), SUB (VAR "x", VAR "y"))))) in
  Problem4.translate(p4_t2);;

  (* Test for Problem 5 *)

  let p5_t1 = Problem4.(LET ("x", CONST 1, VAR "x")) in
  Problem5.nl_run(Problem4.translate p5_t1);;

  let p5_t2 = Problem4.(LET ("x", CONST 7, LET ("y", CONST 2, LET ("y", LET ("x", SUB(VAR "x", CONST 1),  SUB (VAR "x", VAR "y")), SUB (SUB (VAR "x", CONST 8), VAR "y"))))) in
Problem5.nl_run(Problem4.translate p5_t2);;
*)
