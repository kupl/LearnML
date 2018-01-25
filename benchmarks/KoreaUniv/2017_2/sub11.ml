(*problem 1*)
type btree = Empty |Node of int * btree * btree

let rer mirror : btree -> btree = fun t ->
match t with
|Empty -> Empty
|Node(l,v,r) -> Node(l,mirror r, mirror v);;

(*problem 2*)
type nat = ZERO|SUCC of nat;;

let rec  natadd : nat -> nat -> nat= fun n1 n2 ->
match n1 with
|ZERO-> n2
|SUCC m -> SUCC(natadd m n2);;

let rec natmul : nat -> nat-> nat = fun n1 n2 ->
match n2 with
|ZERO -> ZERO
|SUCC m -> natadd (natmul n1 m) (n1);;

let rec natexp : nat -> nat -> nat = fun n1 n2->
match n2 with
|ZERO ->SUCC(ZERO)
|SUCC m-> natmul(natexp n1 m) (n1);;

(*problem3*)

type formula=
True
|False
|Var of string
|Neg of formula
|And of formula*formula
|Or of formula * formula
|Imply of formula * formula
|Iff of formula * formula;;

let rec sat1 : formula -> formula 
= fun t -> match t with
|True -> True
|False-> False
|Var s -> Var s
|Neg(t1) -> if (sat1 t1)=True then False else 
if (sat1 t1)=False then True else Neg(sat1 t1)
|And(t1,t2) -> if (sat1 t1)=(sat1 (Neg (t1))) then False
else if ((sat1 t1) = True && (sat1 t1) = True) then True
else if ((sat1 t1) = False && (sat1 t2) = False ) then False
else True
|Or(t1,t2) -> if ((sat1 t1)=False && (sat1 t2)= False) then 
False else True
|Imply(t1,t2) -> if ((sat1 t1)=True && (sat1 t2)=False) then 
False else True
|Iff(t1,t2) -> if (sat1 t1 = sat1 t2) then True else False
;;


let rec sat : formula -> bool= fun f ->
match f with
|True -> true
|False -> false
|Var (a) -> true
|_ -> sat (sat1 f);;

(*problem4*)

type aexp =
|Const of int
|Var of string
|Power of string * int
|Times of aexp list
|Sum of aexp list;;

let rec diff :aexp * string -> aexp = fun(e,x)->
match e with
|Const a-> Const 0
|Var s -> if x = s then Const 1 else Const 0
|Power (s,i)-> if x = s then match i with
|1-> Const 1
|0 -> Const 0
|_ -> Times[Const i;Power(x,i-1)] else Const 0
|Times (l) -> (match l with
    |[]->Const 0
    |h::t -> Sum [Times ([diff(h,x)] @ t);Times (h::[diff(Times t,x)])])
|Sum l -> (match l with 
    |[] -> Const 0
    |h::t -> Sum[diff(h,x);diff(Sum t,x)]);;

(*problem5*)

type exp =X
|INT of int
|ADD of exp *exp
|SUB of exp *exp
|MUL of exp *exp
|DIV of exp * exp
|SIGMA of exp*exp*exp;;

let rec calculator : exp -> int = fun e ->
match e with
|X -> calculator X
|INT a->a
|ADD(a,b)-> ((calculator a)+(calculator b))
|MUL(a,b)-> ((calculator a) *  (calculator b))
|SUB(a,b)-> ((calculator a) - (calculator b))
|DIV(a,b)-> ((calculator a) / (calculator b))
|SIGMA(a,b,c)-> (match a,b,c with 
    |INT n1 , INT n2, _ ->
    if(n1<n2+1) then
    (match c with
     |X -> n1 + calculator(SIGMA(INT(n1+1), b,c))
     |INT a -> a + calculator(SIGMA(INT(n1+1),b,c))
     |ADD (d,e) -> calculator(SIGMA(a,a,d))+calculator(SIGMA(a,a,e)) + calculator(SIGMA(INT (n1+1),b,c))
     |SUB (d,e) -> calculator(SIGMA(a,a,d)) - calculator(SIGMA(a,a,e)) + calculator(SIGMA(INT (n1+1),b,c))
     |MUL (d,e) ->calculator(SIGMA(a,a,d)) * calculator(SIGMA(a,a,e)) + calculator(SIGMA(INT(n1+1),b,c))
     |DIV (d,e) -> calculator(SIGMA(a,a,d)) / calculator(SIGMA(a,a,e)) + calculator(SIGMA(INT(n1+1),b,c))
     |SIGMA(d,e,f) -> raise(Failure("sigma is too many")))
    else 0
    |_-> raise(Failure("error")));;

(*problem 6*)

type mobile =
branch *branch
and branch = SimpleBranch of length *weight
|CompoundBranch of length * mobile
and length = int
and weight = int;;
 
let rec baleq m =
match m with
|(SimpleBranch(ll,wl),SimpleBranch(lr,wr))->wl+wr
|(SimpleBranch(ll,wl),CompoundBranch(lr,mr))->(wl+(baleq mr))
|(CompoundBranch(ll,ml),SimpleBranch(lr,wr))->((baleq ml)+wr)
|(CompoundBranch(ll,ml),CompoundBranch(lr,mr))->((baleq ml)+(baleq mr));;

let rec balanced : mobile -> bool  = fun m->
match m with
|(SimpleBranch(a,b),SimpleBranch(c,d))->if ((a*b) =( c*d)) then true else false
|(SimpleBranch(a,b),CompoundBranch(c,d))->if (((a*b)=(c*baleq (d)))&&balanced d) then true else false
|(CompoundBranch(a,b),SimpleBranch(c,d))-> if (((c*d)=(a*baleq (b)))&&balanced b)then true else false
|(CompoundBranch(a,b),CompoundBranch(c,d))->if ((c*baleq d)=(a*baleq b)) && balanced b && balanced d then true else false;;

(*problem7*)

type digit = ZERO
|ONE

type bin = digit list

let rec length : bin -> int = fun b ->
match b with
|[] -> 0
|h::t -> 1 + (length t);;

let rec power : int -> int = fun n ->
if n = 0 then 1 else 2 * (power (n-1));;

let rec bd : bin -> int = fun b ->
match b with 
|[] -> 0
|[ONE] -> 1
|[ZERO] -> 0
|ONE::t -> (power ((length b)-1)) + (bd t)
|ZERO::t -> bd t;;

let rec db : int -> bin = fun n ->
if n=0 then [ZERO] else if n=1 then [ONE]
else if (n mod 2 = 1) then (db (n/2))@ [ONE]
else if (n mod 2 =1) then (db (n/2)) @ [ONE]
else (db (n/2)) @ [ZERO];;

let bmul : bin -> bin -> bin = fun b1 b2 ->
db((bd b1) * (bd b2));;
