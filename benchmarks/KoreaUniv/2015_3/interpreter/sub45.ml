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

