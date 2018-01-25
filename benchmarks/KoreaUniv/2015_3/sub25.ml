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
  


let rec sumOfWeight
=fun br ->
   match br with
      | SimpleBranch(l,w)->w
      | CompoundBranch(l,(lb,rb))->(sumOfWeight lb)+(sumOfWeight rb);;


let multiple 
=fun br -> match br with
    | SimpleBranch (l, w) -> l * w
    | CompoundBranch (l, (lb,rb)) -> l * ((sumOfWeight lb) + (sumOfWeight rb));;

let rec balanced : mobile -> bool
=fun (lb,rb) ->match lb with
                            | SimpleBranch (l1, w1) -> (match rb with
                                                      | SimpleBranch (l2, w2) -> if ((multiple lb) = (multiple rb)) then true
                                                                                  else false
                                                      | CompoundBranch (l2,(lb1,rb1)) -> if(( balanced (lb1, rb1) = true) && ((multiple lb) = (multiple rb))) then true
                                                                                        else false)
                            | CompoundBranch (l1, (lb1, rb1)) ->  if( balanced (lb1, rb1) = false) then false
                                                                  else (match rb with
                                                                      | SimpleBranch (l2,w2) -> if ((multiple lb) = (multiple rb)) then true
                                                                                                else false
                                                                      | CompoundBranch (l2, (lb2, rb2)) -> if (balanced (lb2, rb2) = false) then false
                                                                                                           else if ((multiple lb) = (multiple rb)) then true
                                                                                                         else false);;
                                                                  
end






(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct 
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  

  let check : exp -> bool
  =fun e -> true;;


  let rec isFreeVar : var * string list -> bool 
  =fun (v, l) -> match l with
                            | [] -> false
                            | hd :: tl -> if (v = hd) then true else isFreeVar (v,tl);;

  let rec findVar : exp * string list -> bool
  = fun (e, l) -> match e with
                  | V ((a:var)) -> isFreeVar (a, l)
                  | P ((v:var),e1) -> findVar (e1, (l @ [v]))
                  | C (e1,e2) -> if (findVar(e1,l) && findVar(e2,l))= true then true else false;;


  let check  : exp -> bool
  =fun e -> findVar (e, []);;

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
  

  let rec eval : exp -> env -> value
=fun exp env -> match exp with 
                | CONST n -> Int n 
                | VAR x -> apply_env env x 
                | ADD (e1,e2) -> let v1 = eval e1 env in let v2 = eval e2 env in 
                (match v1,v2 with 
                        | Int n1, Int n2 -> Int (n1 + n2) 
                        | _ -> raise (Failure "Type Error: non-numeric values")) 
                | SUB (e1,e2) -> 
                        let v1 = eval e1 env in let v2 = eval e2 env in 
                            (match v1,v2 with 
                            | Int n1, Int n2 -> Int (n1 - n2) 
                            | _ -> raise (Failure "Type Error: non-numeric values"))
            | ISZERO e -> (match eval e env with 
                            | Int n when n = 0 -> Bool true 
                            | _ -> Bool false) 
            | IF (e1,e2,e3) -> 
            (match eval e1 env with 
                    | Bool true -> eval e2 env 
                    | Bool false -> eval e3 env 
                    | _ -> raise (Failure "Type Error: condition must be Bool type")) 
            | LET (x,e1,e2) -> let v1 = eval e1 env in eval e2 (extend_env (x,v1) env)
            | LETREC (f,x,e1,e2) -> eval e2 (extend_env (f,RecProcedure(f,x,e1,env)) env)
            | PROC(x,e1) -> Procedure (x,e1,env)          
            | CALL (e1,e2)->(match eval e1 env with
                            | Procedure (x,e,env1)->(match eval e2 env with 
                                                    |v2-> eval e (extend_env (x,v2) env1))
                            | RecProcedure (f,x,e,env1)->(match eval e2 env with 
                                                          |v2->eval e (extend_env (x,v2) (extend_env (f,RecProcedure (f,x,e,env1)) env1)))  
                            |_->raise (Failure "Type Error: condition must be Procedure") )

let run : program -> value
  =fun pgm -> eval pgm empty_env;;

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

end

(***********************************)
(**            Problem 5          **)
(***********************************)

module Problem5 = struct
  open Problem4
  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list;;
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> NL_Int 0;;


 let rec nl_eval : nl_exp -> nl_env -> nl_value
=fun exp env -> match exp with 
                | NL_CONST n -> NL_Int n 
                | NL_VAR v -> List.nth env v 
                | NL_ADD (e1,e2) -> let v1 = nl_eval e1 env in let v2 = nl_eval e2 env in 
                (match v1,v2 with 
                        | NL_Int n1, NL_Int n2 -> NL_Int (n1 + n2) 
                        | _ -> raise (Failure "Type Error: non-numeric values")) 
                | NL_SUB (e1,e2) -> 
                        let v1 = nl_eval e1 env in let v2 = nl_eval e2 env in 
                            (match v1,v2 with 
                            | NL_Int n1, NL_Int n2 -> NL_Int (n1 - n2) 
                            | _ -> raise (Failure "Type Error: non-numeric values"))
            | NL_ISZERO e -> (match nl_eval e env with 
                            | NL_Int n when n = 0 ->NL_Bool true 
                            | _ -> NL_Bool false) 
            | NL_IF (e1,e2,e3) -> 
            (match nl_eval e1 env with 
                    | NL_Bool true -> nl_eval e2 env 
                    | NL_Bool false -> nl_eval e3 env 
                    | _ -> raise (Failure "Type Error: condition must be Bool type")) 
            | NL_LET (e1,e2) -> let x = nl_eval e1 env in nl_eval e2 (x::env)
            | NL_PROC(v,e1) -> NL_Procedure (e1,env)          
            | NL_CALL (e1,e2)-> match nl_eval e1 env with
                            | NL_Procedure (e,env1)-> let v = nl_eval e2 env in
                            nl_eval e (v::env1);;

let nl_run : nl_program -> nl_value
  =fun pgm -> (nl_eval pgm []);;
end
