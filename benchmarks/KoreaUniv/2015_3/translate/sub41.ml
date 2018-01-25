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

  let rec nth l n =
  match l with
    | hd::tl -> if hd=n then 0 else 1 + nth tl n
    | [] -> raise (Failure "Error: given expression contains free variable!")

  let translate : program -> nl_program = fun pgm ->
  let envr = ref [] in
  let rec trans pgm1 =
  (match pgm1 with
    | CONST n -> NL_CONST n
    | VAR v -> NL_VAR (nth !envr v)
    | ADD (e1,e2) -> NL_ADD (trans e1,trans e2)
    | SUB (e1,e2) -> NL_SUB (trans e1,trans e2)
    | ISZERO e -> NL_ISZERO (trans e)
    | IF (e1,e2,e3) -> NL_IF (trans e1,trans e2,trans e3)
    | LET (x,e1,e2) -> (match e1 with 
	| CONST v1 -> (envr:=[x] @ !envr) ; NL_LET (trans e1,trans e2)
	| _ -> (fun x -> (match x with
		| NL_LET(e11,e22) -> NL_LET(e22,e11)
		| _ -> raise (Failure "This never happens!"))) (NL_LET (((envr:=[x] @ !envr) ; (trans e2)),trans e1)))
    | PROC (x,e) -> (envr:=[x] @ !envr ; NL_PROC (trans e))
    | CALL (e1,e2) -> NL_CALL (trans e1,trans e2)) in trans pgm
