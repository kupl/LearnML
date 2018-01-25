(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with|Empty->Empty|Node(root,left,right)->Node(root,mirror(right),mirror(left));;


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n2 with|SUCC(x)->(natadd (SUCC(n1)) x)|ZERO->n1;;

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with|ZERO->ZERO|SUCC(x)->natadd n2 (natmul x n2);;

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with|ZERO->SUCC(ZERO)|SUCC(x)->if x=ZERO then n1 else natmul n1 (natexp n1 x);;


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


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list


let rec getlast lst size = match lst with|[]->raise(Failure "too short")|hd::tl->if size=1 then hd else getlast tl (size-1);;

let rec changelist l n e = match l with|[]->raise(Failure "too short")|hd::tl->if n=0 then e::tl else hd::changelist tl (n-1) e;;
let rec deletelist l n = match l with|[]->raise(Failure "too short")|hd::tl->if n=0 then tl else hd::deletelist tl (n-1);;

let rec diffCal : bool->aexp list->aexp list->string->int->aexp list = fun ty e t v count->
if ty=true then
let n = (length e)-(length t) in
(match t with|[]->if count=0 then [Const 0] else []
|hd::tl->(match hd with
|Const m->(diffCal true e tl v count)
|Var x->if v=x then 
	(if (length e)=1 then [(Const 1)]
		else (Times (deletelist e n)::(diffCal true e tl v (count+1))))
 else (diffCal true e tl v count)
|Power (x,m)->if v=x then
(if m=2 then Times (changelist e n (Times [Const 2;Var "x"]))::(diffCal true e tl v (count+1))
	else if m=1 then
	(if (length e)=1 then [(Const 1)]
		else Times (deletelist e n)::(diffCal true e tl v (count+1)))
else if m=0 then (diffCal true e tl v count)
else Times (changelist e n (Times [Const m;Power (x,m-1)]))::(diffCal true e tl v (count+1))
 ) else (diffCal true e tl v count)
|Times lst->diffCal true ((deletelist e n)@lst) (tl@lst) v count
|Sum lst->
let rst = (diffCal false lst lst v 0) in
if((getlast rst (length rst))=(Const 0)) then
Times (changelist e n (Sum (deletelist rst ((length rst)-1))))::(diffCal true e tl v count)
else Times (changelist e n (Sum rst))::(diffCal true e tl v (count+1))
))
else
match t with|[]->if count=0 then [Const 0] else []|hd::tl->(match hd with
|Const n->if (length e)=1 then (Const 0)::(diffCal false e tl v count) else (diffCal false e tl v count)
|Var x-> if v=x then (Const 1)::diffCal false e tl v (count+1)
else if (length e)=1 then (Const 0)::diffCal false e tl v count else diffCal false e tl v count
|Power (x,n)->if x=v then (
if n=2 then (Times [Const 2; Var "x"])::(diffCal false e tl v (count+1))
else if n=1 then (Const 1)::(diffCal false e tl v (count+1))
else if n=0 then (diffCal false e tl v count)
else (Times [Const n;Power(x,n-1)])::(diffCal false e tl v (count+1))
)
 else if (length e)=1 then (Const 0)::(diffCal false e tl v count) else (diffCal false e tl v count)
|Times lst->
let rst = (diffCal true lst lst v 0) in
if (getlast rst (length rst))=(Const 0) then
if (length e)=1 then (Const 0)::(diffCal false e tl v count)
else (diffCal false e tl v count)
else rst@(diffCal false e tl v 1)
|Sum lst->diffCal false e (tl@lst) v count
);;


let rec diff : aexp * string -> aexp = fun (e,x)->
match e with|Var n->if n=x then Const 1 else Const 0|Const n->Const 0
|Power (a,b)->if a=x then (
if b=1 then (Const 1)
else if b=0 then (Const 0)
else (Times [Const b;Power(a,b-1)])
 ) else (Const 0)
|Times k->let rst = (diffCal true k k x 0) in
	if (getlast rst (length rst))=(Const 0) then (Const 0)
	else Sum rst
|Sum k->let rst = (diffCal false k k x 0) in
	Sum rst;;


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let rec get : exp->int->int = fun exp v->
match exp with|X->v|INT n->n
|ADD (n1,n2)->let a = (get n1 v) in let b = (get n2 v) in (a+b)
|SUB (n1,n2)->let a = (get n1 v) in let b = (get n2 v) in (a-b)
|MUL (n1,n2)->let a = (get n1 v) in let b = (get n2 v) in (a*b)
|DIV (n1,n2)->let a = (get n1 v) in let b = (get n2 v) in (a/b)
|SIGMA (x,y,e)->let a = (get x v) in let b = (get y v) in
	if a>b then 0 else (get e a)+get (SIGMA (INT (a+1),INT b,e)) (a+1);;

let rec calculator : exp -> int
= fun e -> match e with|X-> raise (Failure "Type Error")|INT n->n
|ADD (n1,n2)->let a = calculator n1 in let b = calculator n2 in (a+b)
|SUB (n1,n2)->let a = calculator n1 in let b = calculator n2 in (a-b)
|MUL (n1,n2)->let a = calculator n1 in let b = calculator n2 in (a*b)
|DIV (n1,n2)->let a = calculator n1 in let b = calculator n2 in (a/b)
|SIGMA (n1,n2,s)->let a = calculator n1 in get e a;;


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let front t = match t with|(x,_)->x;;
let rear t = match t with|(_,y)->y;;

let rec wei :branch->int = fun branch->match branch with
|SimpleBranch (l,w)-> w
|CompoundBranch (l,m)->(match m with
	|(x,y)->(wei x)+(wei y)
);;

let rec eval : branch->int*bool = fun branch->
match branch with
|SimpleBranch (l,w)->(l*w,true)
|CompoundBranch (l,m)->(match m with
|(a,b)->let x= (front (eval a)) in let y = (front (eval b)) in
if (rear (eval a)) = false then (0,false) else if (rear (eval b)) = false then (0,false)
else if (x=y) then ((l*(wei a+wei b)),true) else (0,false));;

let rec balanced : mobile -> bool
= fun m -> match m with|(a,b)->if (rear (eval a))=false then false
else if (rear (eval b))=false then false
else if (front (eval a))=(front (eval b)) then true else false;;

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec binexp n = if n>0 then 2*binexp(n-1) else 1;;

let rec bintodec : bin->int
= fun n -> match n with|[]->0|hd::tl->if hd=ONE
then (binexp (length n-1))+(bintodec tl) else (bintodec tl);;

let rec dectobin : int->bin
= fun n -> if n>0 then (if (n mod 2) = 1
	then (dectobin (n/2))@[ONE] else (dectobin (n/2))@[ZERO])
else [];;

let bmul : bin -> bin -> bin
= fun b1 b2 -> let x=((bintodec b1)*(bintodec b2)) in
if x=0 then [ZERO] else dectobin x;;





