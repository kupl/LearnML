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
