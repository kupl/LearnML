(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
  match exp with
  | Const _ -> Const 0
  | Var x -> if x = var then Const 1 else Const 0
  | Power (x,y) -> if x = var then Times[Const (y);Power(x,y-1)] else Const 0
  | Times l ->
    (match l with
    | [] -> Const 0
    | hd::tl -> Sum [Times (diff (hd,var)::tl); Times [hd; diff (Times tl,var)]] )
  | Sum l -> 
    (match l with
    | [] -> Const 0
    | hd::tl -> Sum [diff (hd,var); diff (Sum tl,var)])
end

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec eval_w m = 
  match m with
  |(SimpleBranch(a,b),SimpleBranch(c,d)) -> b + d
  |(SimpleBranch(a,b),CompoundBranch(c,d)) -> b + (eval_w d)
  |(CompoundBranch(a,b),SimpleBranch(c,d)) -> (eval_w b) + d
  |(CompoundBranch(a,b),CompoundBranch(c,d)) -> (eval_w b) + (eval_w d)

  let rec balanced : mobile -> bool
  = fun mob -> 
  match mob with
  |(SimpleBranch(a,b),SimpleBranch(c,d)) -> if a * b = c * d then true else false
  |(SimpleBranch(a,b),CompoundBranch(c,d)) -> if (a * b = c * (eval_w d)) && balanced d then true else false
  |(CompoundBranch(a,b),SimpleBranch(c,d)) -> if balanced b && (a * (eval_w b) = c * d) then true else false
  |(CompoundBranch(a,b),CompoundBranch(c,d)) -> if a * (eval_w b) = c * (eval_w d) && balanced b && balanced d then true else false

end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  type env = int list

  let empty_env = []
  let lookup_env env =
    match env with 
    | [] -> raise (Failure "empty env")
    | hd::tl -> hd
  let extend_env env v = v::env

  let rec eval env exp =
  match exp with
  | X -> lookup_env env
  | INT x -> x
  | ADD (x,y) -> (eval env x) + (eval env y)
  | SUB (x,y) -> (eval env x) - (eval env y)
  | MUL (x,y) -> (eval env x) * (eval env y)
  | DIV (x,y) -> (eval env x) / (eval env y)
  | SIGMA (e1,e2,e3) ->  
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    let rec sigma env (v1,v2,e) = 
      if v1 > v2 then 0
      else if v1 = v2 then let env' = extend_env env v2 in eval env' e
      else let env' = extend_env env v1 in (eval env' e) + sigma env' (v1+1,v2,e) in
    sigma env (v1,v2,e3)

  let rec calculator : exp -> int
  = fun exp -> eval empty_env exp 
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  type env = var list

  let empty_env = []

  let rec lookup_env env v = 
    match env with
    | [] -> false
    | hd::tl -> if hd = v then true else lookup_env tl v

  let extend_env env var = var::env

  let check : exp -> bool
  = fun exp -> 
  let rec check_env env e = 
  match e with
  | V x -> lookup_env env x
  | P (x,y) -> let env' = extend_env env x in check_env env' y
  | C (x,y) -> if check_env env x && check_env env y then true else false
  in check_env empty_env exp
end