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
  
  let rec getWeight : mobile -> int
=fun (lb,rb) ->
(match lb with
SimpleBranch(l,w) -> w |
CompoundBranch(l,m) -> getWeight(m)) +
(match rb with
SimpleBranch(l,w) -> w |
CompoundBranch(l,m) -> getWeight(m));;

let rec cal : branch -> int
=fun br -> (match br with
SimpleBranch(l,w) -> l*w|
CompoundBranch(l,m) -> if balanced(m) then l*getWeight(m) else 0)

and balanced : mobile -> bool
=fun (lb,rb) -> if cal(lb)=cal(rb) && cal(lb)>0 && cal(rb)>0 then true else false;;
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec look : var * string list -> bool
=fun (v,li) -> match li with
[] -> false |
h::t -> if h=v then true else look(v,t);;

let rec add : var * string list -> string list
=fun (v, li) -> match li with
[] -> li@[v] |
h::t -> if h=v then li else [h]@add(v,t);;

let rec collect1: exp * string list-> string list
=fun (e,li) -> match e with
P(v,e1) -> collect1 (e1,add(v,li)) |
C(e1,e2) -> collect1 (e2,collect1(e1,li)) |
V v1 -> li;; 

let rec collect2: exp * string list-> string list
=fun (e,li) -> match e with
P(v,e1) -> collect2(e1,li) |
C(e1,e2) -> collect2 (e2,collect2(e1,li)) |
V v1 -> add(v1,li);;

let rec compare: string list * string list-> bool
=fun (li1,li2) -> match li2 with
[] -> true |
h::t -> look(h,li1)&&compare(li1,t);;

let check : exp -> bool
=fun e -> let li1= collect1(e,[]) in
  let li2=collect2(e,[]) in compare (li1,li2);;
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
  CONST i -> Int i
| VAR v -> apply_env env v
| ADD (e1,e2) ->
  let v1 = eval e1 env in
  let v2 = eval e2 env in
    (match v1,v2 with
    | Int n1, Int n2 -> Int (n1+n2)
    | _ -> raise (Failure "Type Error at ADD"))
| SUB (e1,e2) ->
  let v1 = eval e1 env in
  let v2 = eval e2 env in
    (match v1,v2 with
    | Int n1, Int n2 -> Int (n1-n2)
    | _ -> raise (Failure "Type Error at ADD"))
| ISZERO e ->let e1=eval e env in
  (match e1 with
  | Int x -> if x=0 then Bool true else Bool false
  | _ -> raise (Failure "Type Error at ISZERO"))
| IF (e1,e2,e3) -> let v1=eval e1 env in
  (match v1 with
  | Bool b -> if b=true then eval e2 env else eval e3 env
  | _ -> raise (Failure "Type Error at IF"))
| LET (v1,e1,e2) -> let v2= eval e1 env in eval e2 (extend_env (v1,v2) env)
| LETREC (v1,v2,e1,e2) -> let v3=RecProcedure(v1,v2,e1,env) in eval e2 (extend_env (v1,v3) env)
| PROC (v,e) -> Procedure (v,e,env)
| CALL (e1,e2) -> (match eval e1 env with
  | Procedure(x,e,env2) -> let v=eval e2 env in eval e (extend_env (x,v) env2)
  | RecProcedure(x1,x2,e,env2) -> let v=eval e2 env in
    let v3=RecProcedure(x1,x2,e,env2) in
      let env3=extend_env (x1,v3) env2 in eval e (extend_env (x2,v) env3)
  | _ -> raise (Failure "Type Error at CALL"))


let rec run : program -> value
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

  let rec work : var * var list * int -> nl_exp
  =fun (v,li,n) -> match li with
  |[] -> raise (Failure "Environment Search Error")
  |h::t -> if v=h then NL_VAR n else work(v,t,n+1)

  let find : var * var list -> nl_exp
  =fun (v,li) -> work (v,li,0)

  let rec tt : program -> var list -> nl_program
  =fun pgm li -> match pgm with
  CONST i -> NL_CONST i
| VAR i -> find (i,li)
| ADD (e1,e2) -> NL_ADD(tt e1 li,tt e2 li)
| SUB (e1,e2) -> NL_SUB(tt e1 li,tt e2 li)
| ISZERO e -> NL_ISZERO (tt e li)
| IF (e1,e2,e3) -> NL_IF(tt e1 li,tt e2 li,tt e3 li)
| LET(v,e1,e2) -> let li'=[v]@li in NL_LET(tt e1 li,tt e2 li')
| PROC(v,e) -> let li'=[v]@li in NL_PROC(tt e li')
| CALL(e1,e2) -> NL_CALL(tt e1 li ,tt e2 li)
  

  let translate : program -> nl_program
=fun pgm -> tt pgm []
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


let rec find2 : int * nl_value list -> nl_value
=fun (index,li) -> match li with
 |[] -> raise (Failure "Environment Search Error")
 |h::t -> if index=0 then h else find2(index-1,t)

let rec rr : nl_program -> nl_env -> nl_value
=fun nl_exp nl_env -> match nl_exp with
 NL_CONST i -> NL_Int i
|NL_VAR i -> find2 (i,nl_env)
|NL_ADD (e1,e2) ->
  let v1= rr e1 nl_env in
  let v2= rr e2 nl_env in
    (match v1,v2 with
    | NL_Int n1,NL_Int n2 -> NL_Int (n1+n2)
    | _ -> raise (Failure "Type ERROR AT NL_ADD"))
|NL_SUB (e1,e2) ->
  let v1= rr e1 nl_env in
  let v2= rr e2 nl_env in
    (match v1,v2 with
    | NL_Int n1,NL_Int n2 -> NL_Int (n1-n2)
    | _ -> raise (Failure "Type ERROR AT NL_SUB"))
|NL_ISZERO e -> let e1=rr e nl_env in
  (match e1 with
  | NL_Int x -> if x=0 then NL_Bool true else NL_Bool false
  | _ -> raise (Failure "Type Error at NL_ISZERO"))
|NL_IF (e1,e2,e3) -> let v1=rr e1 nl_env in
  (match v1 with
  | NL_Bool b -> if b=true then rr e2 nl_env else rr e3 nl_env
  | _ -> raise (Failure "Type Error at NL_IF"))
|NL_LET (e1,e2) -> let nl_env'=[rr e1 nl_env]@nl_env in rr e2 nl_env'
|NL_PROC e -> NL_Procedure(e,nl_env)
|NL_CALL (e1,e2) -> (match rr e1 nl_env with
  |NL_Procedure(e,nl_env') -> let nl_env2=[rr e2 nl_env]@nl_env' in rr e nl_env2
  | _ -> raise (Failure "Type Error at NL_CALL"))


let nl_run : nl_program -> nl_value
=fun pgm -> rr pgm []
end
