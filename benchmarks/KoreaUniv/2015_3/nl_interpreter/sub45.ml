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

