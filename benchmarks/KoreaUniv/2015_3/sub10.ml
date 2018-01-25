(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)
(*2014210077 김준영*)
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
  =fun (lb,rb) -> false 

  let rec getw m=
  match m with 
  | (l,r)->
   match l with 
    |SimpleBranch(a,b)->
     (match  r with
     | SimpleBranch(c,d)-> b+d
     | CompoundBranch(e,f)-> getw(f)+b
    )
    |CompoundBranch(a,b)-> 
    (match  r with
     | SimpleBranch(c,d)-> getw(b)+d
     | CompoundBranch(e,f)-> getw(f)+getw(b)
    )

let rec balanced m =
  match m with 
  |(l,r)-> 
   (match l with 
    | SimpleBranch (a,b) ->
     (match r with 
      |SimpleBranch(len,weight)-> 
      if len*weight= a*b then true else false
      |CompoundBranch(c,d)-> 
      if balanced(d) then 
        if c*getw(d)= a*b then true else false
      else false 
    )
    | CompoundBranch(e,f)->
    if balanced(f) then
     (match r with 
      |SimpleBranch(len,weight)-> 
      if len*weight= e*getw(f) then true else false
      |CompoundBranch(c,d)-> 
      if c*getw(d)= e*getw(f) then true else false
    )
    else false
    )

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
  =fun e -> true 

  let rec contain ex l=
  match ex with 
  |V(v)-> if List.mem v l then true else false
  |P(v,y)-> contain y (l@[v])
  |C(exp1,exp2) -> (contain exp1 l)&&(contain exp2 l)

let check ex =
  contain ex []
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
  
  let run : program -> value
  =fun pgm -> Int 0 (* TODO *)

  let rec eval_bop : (int -> int -> int) -> exp -> exp -> env -> value
  =fun op e1 e2 env ->
  let v1 = eval(e1,env) in
  let v2 = eval(e2,env) in
    (match v1,v2 with
    | Int n1, Int n2 -> Int (op n1 n2)
    | _ -> raise (Failure "Type Error: non-numeric values"))

and eval : exp * env -> value
=fun (exp,env) ->
  match exp with
  | CONST n -> Int n
  | VAR x -> apply_env env x
  | ADD (e1,e2) -> eval_bop (+) e1 e2 env
  | SUB (e1,e2) -> eval_bop (-) e1 e2 env
  | ISZERO e ->
   let v= eval(e,env) in 
     (match v with 
      |Bool b-> raise (Failure ("error"))
      |Int n -> if n=0 then Bool true else Bool false)
  | IF (e1,e2,e3) ->
    (match eval(e1,env) with
    | Bool true -> eval(e2,env)
    | Bool false -> eval(e3,env)
    | _ -> raise (Failure "Type Error: condition must be Bool type"))
  | LET (x,e1,e2) ->(
    let v1 = eval(e1,env) in
      eval (e2,(extend_env (x,v1) env))
    )
  | LETREC(f,x,e1,e2) -> (
    let v=RecProcedure(f,x,e1,env) in 
    eval(e2,(extend_env (f,v) env)))
  | PROC(x,e)-> Procedure(x,e,env) 
  | CALL(e1,e2)-> 
    let v1=eval(e1,env) in
     (match v1 with 
      |RecProcedure(f,x,exp1,env1)-> 
        eval(LET(x,e2,exp1),env)
      |Procedure(x,e,env1)-> 
        eval(LET(x,e2,e),env)
     )


     

let run : program -> value
=fun pgm -> eval(pgm,empty_env)

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
  
  let translate : program -> nl_program
  =fun pgm -> NL_CONST 0 

  let find_index elt lst =
    let rec find_it elt acc = function
    | hd :: tl when elt = hd -> acc 
    | hd :: tl -> find_it elt (acc + 1) tl 
    | _ -> raise Not_found 
  in find_it elt 0 lst 

  let rec trans (pgm,envl)=
     match pgm with
     | CONST(a) -> NL_CONST a
     | VAR(x)-> NL_VAR (find_index x envl) 
     | ADD(e1,e2)-> NL_ADD(trans(e1,envl),trans(e2,envl))
     | SUB(e1,e2)-> NL_SUB(trans(e1,envl),trans(e2,envl))
     | ISZERO(e1)-> NL_ISZERO(trans(e1,envl))
     | IF(e1,e2,e3)-> NL_IF(trans(e1,envl),trans(e2,envl),trans(e3,envl))
     | LET(x,e1,e2)-> NL_LET(trans(e1,envl),trans(e2,[x]@envl))
     | PROC(x,e)-> NL_PROC(trans(e,[x]@envl))
     | CALL(e1,e2)-> NL_CALL(trans(e1,envl),trans(e2,envl))

let translate : program ->nl_program
=fun pgm-> trans (pgm,[])

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
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> NL_Int 0 

 let rec nl_eval_bop : (int -> int -> int) -> nl_exp -> nl_exp -> nl_env -> nl_value
  =fun op e1 e2 env ->(
  let v1 = nleval(e1,env) in
  let v2 = nleval(e2,env) in
    (match v1,v2 with
    | NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
    | _ -> raise (Failure "Type Error: non-numeric values")))

 and nleval : nl_exp * nl_env -> nl_value
=fun (exp,env) ->
  match exp with
  | NL_CONST n -> NL_Int n
  | NL_VAR x -> List.nth env x
  | NL_ADD (e1,e2) -> nl_eval_bop (+) e1 e2 env
  | NL_SUB (e1,e2) -> nl_eval_bop (-) e1 e2 env
  | NL_ISZERO e ->
   let v= nleval(e,env) in 
     (match v with 
      |NL_Bool b-> raise (Failure ("error"))
      |NL_Int n -> if n=0 then NL_Bool true else NL_Bool false)
  | NL_IF (e1,e2,e3) ->
    (match nleval(e1,env) with
    | NL_Bool true -> nleval(e2,env)
    | NL_Bool false -> nleval(e3,env)
    | _ -> raise (Failure "Type Error: condition must be Bool type"))
  | NL_LET (e1,e2) ->(
    let v1 = nleval(e1,env) in
      nleval (e2,([v1]@env))
    )
  | NL_PROC(e)-> NL_Procedure(e,env) 
  | NL_CALL(e1,e2)-> 
    let v1=nleval(e1,env) in
     (match v1 with 
      |NL_Procedure(e,env1)-> 
        nleval(NL_LET(e2,e),env)
     )


     

let nl_run : nl_program -> nl_value
=fun nlpgm -> nleval(nlpgm,[])

  
end
