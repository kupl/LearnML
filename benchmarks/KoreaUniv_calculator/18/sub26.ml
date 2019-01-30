type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  and var = String
  
type value = Int of int | Bool of bool;;
type env = (var * value) list;;

let rec lookup_env x e =
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env"))
  | (y,v)::tl -> if x = y then v else lookup_env x tl;;
  
let rec eval_bop : (int -> int -> int) -> exp -> exp -> env -> value
=fun op e1 e2 env ->
  let v1 = calculator e1 env in
  let v2 = calculator e2 env in
    (match v1,v2 with
    | Int n1, Int n2 -> Int (op n1 n2)
    | _ -> raise (Failure "Type Error: non-numeric values"))
    
and calculator env : exp -> int
= fun exp -> 
  match exp with 
    |INT i -> i
    |ADD (a,b) -> eval_bop (+) e1 e2 env
    |SUB (s,t) -> calculator s - calculator t
    |MUL (m,n) -> calculator m * calculator n
    |DIV (d,e) -> calculator d / calculator e
    ;;
    