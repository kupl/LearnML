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
