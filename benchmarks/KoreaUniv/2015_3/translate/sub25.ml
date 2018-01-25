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
  and var = string;;
  
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
    | NL_CALL of nl_exp * nl_exp;;
  

  type value = Int of int | Bool of bool 
             | Procedure of var * exp * env 
             | RecProcedure of var * var * exp * env
  and env = var -> value;;
  
  let empty_env = fun _ -> raise (Failure "Environment is empty");;
  let extend_env (x,v) e = fun y -> if x = y then v else (e y);;
  let apply_env e x = e x;;
  
  let run : program -> value
  =fun pgm -> Int 0;;

 
let run : program -> value
  =fun pgm -> eval pgm empty_env;;


let rec changeVar : string list * var * int -> int
= fun (lst , v, index) -> match lst with
                                | [] -> raise (Failure "Error : Empty Environment")
                                | hd :: tl -> if (v = hd) then index
                                              else (changeVar (tl, v, (index+1)));;



let rec subTrans 
  = fun pgm lst -> match pgm with
                | CONST a -> NL_CONST (a)
                | VAR v -> NL_VAR (changeVar (lst,v,0))
                | ADD (e1,e2) -> NL_ADD (subTrans e1 lst, subTrans e2 lst)
                | SUB (e1,e2) -> NL_SUB (subTrans e1 lst, subTrans e2 lst)
                | ISZERO e -> NL_ISZERO (subTrans e lst)
                | IF (e1, e2, e3) -> NL_IF (subTrans e1 lst, subTrans e2 lst,subTrans e3 lst)
                | LET (v,e1,e2) -> let lst2 = (v :: lst) in NL_LET ((subTrans e1 lst), (subTrans e2 lst2)) 
                | PROC (v,e) -> let lst2 = (v :: lst) in NL_PROC (subTrans e lst2) 
                | CALL (e1, e2) -> NL_CALL (subTrans e1 lst, subTrans e2 lst);;
               

let rec translate : program -> nl_program 
  = fun pgm -> subTrans pgm [];;
