type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

type env = (string * bool) list

let extend_env x b e = (x,b)::e

let rec find_env x e = 
  match e with
  |[] -> raise (Failure "Env error")
  |(id,b)::tl -> if(x=id) then b else find_env x tl

let rec fold f l a =
  match l with
  | [] -> a
  | hd::tl -> f hd (fold f tl a)

let rec is_exist : string list -> string -> bool
= fun l str ->
  match l with
  |[] -> false
  |hd::tl -> (hd=str) || (is_exist tl str)

let rec eval : formula -> env -> bool
= fun f env ->
  match f with
  |True -> true
  |False -> false
  |Var v -> find_env v env
  |Neg f -> not (eval f env)
  |And (f1,f2) -> (eval f1 env) && (eval f2 env)
  |Or (f1,f2) -> (eval f1 env) || (eval f2 env)
  |Imply (f1,f2) -> not (eval f1 env) || (eval f2 env)
  |Iff (f1,f2) -> (eval f1 env) = (eval f2 env)

let rec make_varlist : formula -> string list -> string list
= fun f result->
  match f with
  |Var v -> if(is_exist result v) then result else v::result
  |Neg f -> make_varlist f result
  |And (f1,f2) -> let result = make_varlist f1 result in make_varlist f2 result
  |Or (f1,f2) -> let result = make_varlist f1 result in make_varlist f2 result
  |Imply (f1,f2) -> let result = make_varlist f1 result in make_varlist f2 result
  |Iff (f1,f2) -> let result = make_varlist f1 result in make_varlist f2 result
  |_ -> result

let rec gen_env : string list -> env -> env list
= fun vars env ->
  match vars with
  |[] -> [env]
  |hd::tl -> 
    (gen_env tl (extend_env hd true env)) @ (gen_env tl (extend_env hd false env))

let rec sat : formula -> bool
= fun f ->
  let vars = make_varlist f [] in
  let enumerative_envs = gen_env vars [] in
  fold (fun env r-> r || (eval f env)) enumerative_envs false