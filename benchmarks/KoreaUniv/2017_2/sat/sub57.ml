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

type env = (string*bool)list;;
let extend_env (x,v) e = (x,v)::e;;
let rec scan : formula->env->env
= fun formula env-> match formula with|True->env|False->env
|Var n->extend_env (n,false) env|Neg n->(match n with
|Neg m->scan m env|_->scan n env)|And(n1,n2)->scan n1 (scan n2 env)|
Or(n1,n2)->scan n1 (scan n2 env)|Imply(n1,n2)->scan n1 (scan n2 env)|
Iff(n1,n2)->scan n1 (scan n2 env);;
let rec processenv : env->int->env 
= fun env num-> match env with|[]->[]|hd::tl->(match hd with
|(x,_)->let temp = if (num mod 2) = 0 then false else true in (if num>0 then extend_env (x,temp) (processenv tl (num/2)) else env));;
let rec length lst = match lst with|[]->0|hd::tl->1+length tl;;
let rec find_env : string->env->bool =fun x env-> match env with
|[]->raise(Failure "Error")
|(y,v)::t -> if x=y then v else (find_env x t);;

let rec solve : formula->env->bool 
= fun formula env-> match formula with|True->true|False->false
|Var n->find_env n env|Neg n->(match n with|Neg m->solve m env
|_->let n = solve n env in if n=true then false else true)
|And(n1,n2)->let v1 = solve n1 env in let v2 = solve n2 env in 
(match v1,v2 with|true,true->true|_->false)
|Or(n1,n2)->let v1 = solve n1 env in let v2 = solve n2 env in 
(match v1,v2 with|false,false->false|_->true)
|Imply(n1,n2)->let v1 = solve n1 env in let v2 = solve n2 env in 
(match v1,v2 with|true,false->false|_->true)
|Iff(n1,n2)->let v1 = solve n1 env in let v2 = solve n2 env in if v1=v2 then true else false;;

let square x = x*x;; (*helper function*)
let rec fastexpt:int->int->int = fun b n -> if b=0 then 0
else match n with |0->1|_->if n mod 2 = 0 then square (fastexpt b (n/2)) else b*(fastexpt b (n-1));;

let rec go:int->formula->env->bool = 
	fun num exp env-> if (fastexpt 2 (length env))>num then 
	(if (solve exp (processenv env num))=true then true else (go (num+1) exp env)) else false;;

let sat : formula -> bool
= fun f -> go 0 f (scan f []);;
