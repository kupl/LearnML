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
  = fun (lb,rb) -> let rec bal : mobile -> bool * int
  = fun (lb,rb) -> begin match (lb,rb) with 
                    (SimpleBranch(w,x),SimpleBranch(y,z)) -> if w*x=y*z then (true,x+z) else (false,0)
                  | (CompoundBranch(w,x),SimpleBranch(y,z)) ->begin 
                                                              match bal(x) with 
                                                                (true,ww) -> if w*ww=y*z then (true,ww+z) else (false,0)
                                                              | _ -> (false,0)
                                                              end
                  | (SimpleBranch(y,z),CompoundBranch(w,x)) -> begin
                                                                 match bal(x) with 
                                                                 (true,ww) -> if w*ww=y*z then (true,ww+z) else (false,0)
                                                                | _ -> (false,0) 
                                                                end
                  | (CompoundBranch(y,z),CompoundBranch(w,x)) -> begin 
                                                                  match bal(x),bal(z) with
                                                                  (true,ww),(true,zz) -> if y*zz=w*ww then (true,zz+ww) else (false,0)
                                                                | _ -> (false,0)   
                                                                end
end
in match bal(lb,rb) with 
  (true,x) -> true
| _ -> false
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
  = fun e -> let rec envcheck : exp * exp list -> bool
  = fun (e,l) -> begin
           match e with
            V x -> let rec varcheck : exp list -> bool = fun l2 -> begin match l2 with 
                                                                [] -> false
                                                               | h::t -> if h=V x then true else varcheck(t)
                                                             end  in varcheck(l)  (*check if variable is in the environment return true*)
          | P(v,e) -> envcheck (e,(V v)::l) 
          | C(e1,e2) -> if envcheck (e1,l)&&envcheck (e2,l) then true else false 
          end in envcheck (e,[])
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

let rec eval : exp->env->value 
  = fun exp env -> match exp with 
                | CONST n -> Int n
                | VAR x -> apply_env env x
                | ADD(e1,e2) -> let v1 = eval e1 env in 
                                let v2 = eval e2 env in
                                (match v1,v2 with 
                                  Int n1, Int n2 -> Int (n1+n2)
                                  | _ -> raise (Failure "Type Error: NAN"))
                | SUB(e1,e2) -> let v1 = eval e1 env in 
                                let v2 = eval e2 env in
                                (match v1,v2 with 
                                  Int n1, Int n2 -> Int (n1-n2)
                                  | _ -> raise (Failure "Type Error: NAN"))
                | ISZERO e -> (match eval e env with
                                Int n when n =0 -> Bool true
                              |_ -> Bool false)
                | IF (e1,e2,e3) -> (match eval e1 env with 
                                      Bool true -> eval e2 env
                                    | Bool false -> eval e3 env
                                    | _ -> raise (Failure "Type Error: Condition must be Bool type"))
                | LET (x,e1,e2) -> let v1 = eval e1 env in eval e2 (extend_env (x,v1) env)
                | LETREC (f,x,e1,e2) -> eval e2 (extend_env (f,RecProcedure(f,x,e1,env)) env)
                | PROC(x,e1) -> Procedure(x,e1,env)
                | CALL(e1,e2) -> (match eval e1 env with 
                                    Procedure(x,e,pp) -> let v = eval e2 env in eval e (extend_env (x,v) pp)
                                  | RecProcedure(f,x,e,p) -> let v = eval e2 env in eval e (extend_env (x,v) (extend_env(f,RecProcedure(f,x,e,p)) p))
                                  | _ -> raise (Failure "Type Error: Type Mismatch"))
  
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

let rec helper : program*('a list) -> nl_program
  = fun (pgm2,l) -> match pgm2 with
               CONST c -> NL_CONST c
              | VAR x -> let rec count : ('a list) -> int = fun l -> (match l with
                                                             [] -> 0
                                                           | h::t -> if x=h then 0 else 1+count(t))
                            in NL_VAR (count l)
              | ADD (e1,e2) -> NL_ADD (helper(e1,l),helper(e2,l))
              | SUB (e1,e2) -> NL_SUB (helper(e1,l),helper(e2,l))
              | ISZERO(e1) -> NL_ISZERO (helper(e1,l))
              | IF (e1,e2,e3) -> NL_IF (helper(e1,l),helper(e2,l),helper(e3,l))
              | LET (x,e1,e2) -> NL_LET (helper(e1,l),helper(e2,(x::l)))
              | PROC (x,e1) -> NL_PROC (helper(e1,(x::l)))
              | CALL (e1,e2) -> NL_CALL (helper(e1,l),helper(e2,l))


let translate : program -> nl_program
  = fun pgm -> helper (pgm,[])
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
end