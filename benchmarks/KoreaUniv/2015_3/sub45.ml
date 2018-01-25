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
=fun (lb,rb) ->
  let rec mw : mobile -> int
  = fun (llb , rrb) -> (bw llb ) + (bw rrb)
  and bw : branch -> int
  = fun bb ->
     match bb with
     | SimpleBranch ( l , w) -> w
     | CompoundBranch (l , m) -> mw m in
  let balance_sub_sub : int -> int -> int -> int -> bool
  = fun ll lw rl rw -> (ll * lw) = (rl * rw) in
  let rec balanced_sub : mobile -> bool
  = fun (lb , rb) ->
    match lb , rb with
    | SimpleBranch (ll , lw) , SimpleBranch (rl , rw) -> balance_sub_sub ll lw rl rw
    | SimpleBranch (ll , lw) , CompoundBranch (rl , rm) -> (balance_sub_sub ll lw rl (mw rm)) && (balanced_sub rm)
    | CompoundBranch (ll , lm) , SimpleBranch (rl , rw) -> (balance_sub_sub ll (mw lm)  rl rw) && (balanced_sub lm)
    | CompoundBranch (ll , lm) , CompoundBranch (rl , rm) -> ((balance_sub_sub ll (mw lm) rl (mw rm)) && (balanced_sub lm)) && (balanced_sub rm) in
  balanced_sub (lb , rb)

end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
let rec seek : var -> var list -> bool
= fun v li ->
  match li with
  | [] -> false
  | hd :: tl -> if hd = v then true else seek v tl

let check : exp -> bool
= fun e ->
  let rec check_sub : exp -> var list -> bool
  = fun e li ->
    match e with
    | V v -> seek v li
    | P (v , e) -> check_sub e (v :: li)
    | C (e1, e2) -> (check_sub e1 li) && (check_sub e2 li) in
  check_sub e []


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

  (*helper func 1*)
  let get_int : value -> int
  = fun v ->
    match v with
    | Int n -> n
    | _ -> raise (Failure "get_int function : Inappropriate input")
  (*helper func 2*)
  let is_int : value -> bool
  = fun v ->
    match v with
    | Int n -> true
    | _ -> false
  (*helper func 3*)
  let is_bool : value -> bool
  = fun v ->
    match v with
    | Bool b -> true
    | _ -> false
  (*helper func 4*)
  let get_bool : value -> bool
  = fun v ->
    match v with
    | Bool b -> b
    | _ -> raise (Failure "get_bool function : Inappropriate input")

  let run : program -> value
  = fun pgm ->
    let rec run_sub : env -> exp -> value
    = fun en ex ->
      match ex with
      | CONST n -> Int n
      | VAR v -> apply_env en v
      | ADD (ex1 , ex2) -> let ex1v = run_sub en ex1 in
                           let ex2v = run_sub en ex2 in
                           if (is_int ex1v) && (is_int ex2v)
                             then Int (get_int ex1v + get_int ex2v)
                             else raise (Failure "ADD has not-int expression")
      | SUB (ex1 , ex2) -> let ex1v = run_sub en ex1 in
                           let ex2v = run_sub en ex2 in
                           if (is_int ex1v) && (is_int ex2v)
                             then Int (get_int ex1v - get_int ex2v)
                             else raise (Failure "SUB has not-int expression")
      | ISZERO ex1 -> let ex1v = run_sub en ex1 in
                      if is_int ex1v
                        then Bool ((get_int ex1v) = 0)
                        else raise (Failure "ISZERO has not-int expression")
      | IF (ex1 , ex2 , ex3) ->
          let ex1v = run_sub en ex1 in
          if is_bool ex1v
            then if get_bool ex1v
                   then run_sub en ex2
                   else run_sub en ex3
            else raise (Failure "IF has not-bool expression")
      | LET (v , ex1 , ex2) ->
          let ex1v = run_sub en ex1 in
          run_sub (extend_env (v , ex1v) en) ex2
      | LETREC (v1 , v2 , ex1 , ex2) ->
          let newenv = extend_env (v1 , RecProcedure (v1 , v2 , ex1 , en)) en in
          run_sub newenv ex2
          (* i don't know it's correct*)
      | PROC (v , ex1) -> Procedure (v , ex1 , en)
      | CALL (ex1 , ex2) ->
          let ex1v = run_sub en ex1 in
          let ex2v = run_sub en ex2 in
          (match ex1v with
          | RecProcedure (f , x , ex1ex1 , ex1ex2) ->
              let newenv1 = extend_env (f , ex1v) ex1ex2 in
              let newenv2 = extend_env (x , ex2v) newenv1 in
              run_sub newenv2 ex1ex1
          | _ -> raise (Failure "CALL has non-recprocedure first element")
          )
    in
    run_sub empty_env pgm


(*for test , please make it to comment before submit*)
(*  let pgpgpg =
    LETREC ("double", "x", IF (ISZERO (VAR "x"),
                              CONST 0,
                              ADD (CALL (VAR "double", SUB (VAR "x", CONST 1)) ,
                                   CONST 2)),
        CALL (VAR "double", CONST 6))
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
  
  (*helper func1*)
  let where_exists : var -> var list -> int
  = fun st li ->
      let rec where_exists_sub : var -> var list -> int -> int
      = fun st li n ->
        match li with
        | [] -> raise (Failure "where_exists function : list is empty")
        | hd :: tl -> if hd = st
                        then n
                        else where_exists_sub st tl (n+1) in
      where_exists_sub st li 0


  let translate : program -> nl_program
  =fun pgm ->
    let rec translate_sub : var list -> program -> nl_program
    = fun li pgm ->
      match pgm with
      | CONST n -> NL_CONST n
      | VAR v -> NL_VAR (where_exists v li)
      | ADD (ex1 , ex2) -> let ex1t = translate_sub li ex1 in
                           let ex2t = translate_sub li ex2 in
                           NL_ADD (ex1t , ex2t)
      | SUB (ex1 , ex2) -> let ex1t = translate_sub li ex1 in
                           let ex2t = translate_sub li ex2 in
                           NL_SUB (ex1t , ex2t)
      | ISZERO ex1 -> NL_ISZERO (translate_sub li ex1)
      | IF (ex1 , ex2 , ex3) -> let ex1t = translate_sub li ex1 in
                                let ex2t = translate_sub li ex2 in
                                let ex3t = translate_sub li ex3 in
                                NL_IF (ex1t , ex2t , ex3t)
      | LET (v , ex1 , ex2) -> let ex1t = translate_sub li ex1 in
                               let ex2t = translate_sub (v :: li) ex2 in
                               NL_LET (ex1t , ex2t)
      | PROC (v , ex1) -> NL_PROC (translate_sub (v :: li) ex1)
      | CALL (ex1 , ex2) -> let ex1t = translate_sub li ex1 in
                            let ex2t = translate_sub li ex2 in
                            NL_CALL (ex1t , ex2t)
    in
    translate_sub [] pgm

(*this is for test. please make it to comment before submit*)
(*
let pgpgpg2 =
    LET ("x" , CONST 37 ,
         PROC ( "y" , LET ("z" , SUB (VAR "y" , VAR "x"),
                SUB (VAR "x"  ,VAR "y"))))
let pgpgpg3 =
    LET ("x" , CONST 300,
         SUB (VAR "x" , VAR ("x")))
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
  
  (*helper func1*)
  let nl_env_nth : nl_env -> int -> nl_value
  = fun en n -> List.nth en n
  (*helper func2*)
  let nl_get_int : nl_value -> int
  = fun v -> match v with
             | NL_Int n -> n
             | _ -> raise (Failure "nl_get_int : inappropriate input")
  let nl_is_int : nl_value -> bool
  = fun v -> match v with
             | NL_Int n -> true
             | _ -> false
  let nl_get_bool : nl_value -> bool
  = fun v -> match v with
             | NL_Bool b -> b
             | _ -> raise (Failure "nl_get_bool : inappropriate input")
  let nl_is_bool : nl_value -> bool
  = fun v -> match v with
             | NL_Bool b -> true
             | _ -> false


  let nl_run : nl_program -> nl_value
  =fun pgm ->
    let rec nl_run_sub : nl_env -> nl_program -> nl_value
    = fun en pgm ->
      match pgm with
      | NL_CONST n -> NL_Int n
      | NL_VAR n -> nl_env_nth en n
      | NL_ADD (ex1 , ex2) -> let ex1v = nl_run_sub en ex1 in
                              let ex2v = nl_run_sub en ex2 in
                              if ( nl_is_int ex1v ) && (nl_is_int ex2v)
                                then NL_Int(nl_get_int ex1v + nl_get_int ex2v)
                                else raise (Failure "NL_ADD has not-int expression")
      | NL_SUB (ex1 , ex2) -> let ex1v = nl_run_sub en ex1 in
                              let ex2v = nl_run_sub en ex2 in
                              if (nl_is_int ex1v) && (nl_is_int ex2v)
                                then NL_Int(nl_get_int ex1v - nl_get_int ex2v)
                                else raise (Failure "NL_SUB has not-int expression")
      | NL_ISZERO ex1 -> let ex1v = nl_run_sub en ex1 in
                         if nl_is_int ex1v
                           then NL_Bool ((nl_get_int ex1v) = 0)
                           else raise (Failure "NL_ISZERO has not-int expression")
      | NL_IF (ex1 , ex2 , ex3) -> let ex1v = nl_run_sub en ex1 in
                                   if nl_is_bool ex1v
                                     then if nl_get_bool ex1v
                                            then nl_run_sub en ex2
                                            else nl_run_sub en ex3
                                     else raise (Failure "NL_IF has not-bool expression")
      | NL_LET (ex1 , ex2) -> let ex1v = nl_run_sub en ex1 in
                              let newenv = ex1v :: en in
                              nl_run_sub newenv ex2
      | NL_PROC ex1 -> NL_Procedure (ex1 , en)
      | NL_CALL (ex1 , ex2) ->
          let ex1v = nl_run_sub en ex1 in
          let ex2v = nl_run_sub en ex2 in
          (match ex1v with
          | NL_Procedure (ex1ex , enp) ->
              let newenv = ex2v :: enp in
              nl_run_sub newenv ex1ex
          | _ -> raise (Failure "NL_CALL has non-procedure first element ")
          )
    in
    nl_run_sub [] pgm

(*this is for test. please make it to comment before submit*)
    (*
  let pgpgpg5 = LET ("x" , CONST 1 , VAR "x")
*)



end
