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
  
 let rec helper : nl_program -> nl_env -> nl_value 
 = fun pgm l -> match pgm with
              | NL_CONST i -> NL_Int i
              | NL_VAR i -> let rec findVar : nl_env-> int -> nl_value = fun l i-> (match l with 
                                                                                    [] -> raise (Failure "Empty Environment")
                                                                                  | h::t -> if i=0 then h else findVar t (i-1))
                                            in (findVar l i)
              | NL_ADD (e1,e2) -> let v1 = helper e1 l in 
                                  let v2 = helper e2 l in 
                                  (match  v1,v2 with
                                  NL_Int n1, NL_Int n2-> NL_Int (n1+n2)
                                 | _ -> raise (Failure "Type Error: NAN"))
              | NL_SUB (e1,e2) -> let v1 = helper e1 l  in 
                                  let v2 = helper e2 l  in 
                                  (match  v1,v2 with
                                  NL_Int n1, NL_Int n2-> NL_Int (n1-n2)
                                 | _ -> raise (Failure "Type Error: NAN"))
              | NL_ISZERO (e1) -> (match helper e1 l with
                                NL_Int n when n =0 -> NL_Bool true
                              |_ -> NL_Bool false)
              | NL_IF (e1,e2,e3) -> (match helper e1 l with 
                                      NL_Bool true -> helper e2 l
                                    | NL_Bool false -> helper e3 l
                                    | _ -> raise (Failure "Type Error: Condition must be Bool type"))
              | NL_LET (e1,e2) -> let v1 = helper e1 l in helper e2 (v1::l)
              | NL_PROC (e1) -> NL_Procedure (e1,l)
              | NL_CALL (e1,e2) -> match helper e1 l with 
                                  NL_Procedure(e,p) -> let v = helper e2 l in helper e (v::p)
                                | _ -> raise (Failure "No other Procedure Implemented")


  let nl_run : nl_program -> nl_value
  =fun pgm -> helper pgm []