(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula
;;
type env = (string * bool) list;;
let env = ref [];;

let rec find_env x e = 
match e with
| [] -> true
| (y,v)::tl -> if x=y then false else (find_env x tl);;

let rec put_env f =
match f with
| Var s -> if (find_env s !env)=true then (env:=(s,true)::(!env))
| Neg k -> (put_env k)
| And (x,y) -> ((put_env x);(put_env y))
| Or (x,y) -> ((put_env x);(put_env y))
| Imply (x,y) -> ((put_env x);(put_env y))
| Iff (x,y) -> ((put_env x);(put_env y))
| True -> (env:=(!env))
| False -> (env:=(!env));;

let rec apply_env x e =
match e with
| [] -> true
| (y,v)::tl -> if x=y then v else (apply_env x tl);;

let rec res_form f l= 
match f with
| True -> true
| False -> false
| Var s -> if (apply_env s l)=true then true else false
| Neg k -> not(res_form k l)
| And (x,y) -> (res_form x l)&&(res_form y l)
| Or (x,y) -> (res_form x l)||(res_form y l)
| Imply (x,y) -> (res_form (Or (Neg x,y)) l)
| Iff (x,y) -> (res_form (And (Imply (x,y),Imply(y,x))) l);;

let rec chg_env f e l =
match e with
| []-> if (res_form f l)=true then true else false
| (x,y)::tl -> ((x,true)::l; if (chg_env f tl l)=true then true else false) || ((x,false)::l; if (chg_env f tl l)=true then true else false);;
let sat : formula -> bool
= fun f -> 
((put_env f);(chg_env f (!env) []));;
