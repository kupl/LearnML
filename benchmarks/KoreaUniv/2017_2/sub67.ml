(*problem 1*)
type btree = Empty | Node of int * btree *btree

let rec mirror : btree-> btree
= fun t ->
match t with
|Empty -> Empty
|Node(i,Empty, Empty) -> Node(i, Empty, Empty)
|Node(i, bt1, bt2) -> Node (i, mirror bt1, mirror bt2)

(*problem 2*)
type nat = ZERO | SUCC of nat

let rec inttonat n =
match n with
|0->ZERO
|_-> SUCC(inttonat(n-1))

let rec nattoint n =
match n with
|ZERO -> 0
|SUCC(n1) -> 1+(nattoint n1)


let rec  natadd : nat -> nat -> nat
= fun n1 n2 ->
let a = nattoint n1 in
let b = nattoint n2 in
let c = (a+b) in (inttonat c)

let rec  natmul :nat->nat->nat
= fun n1 n2->
let a = nattoint n1 in
let b = nattoint n2 in
let c= (b*a) in (inttonat c)


let rec exp a b =
if b=0 then 1
else a*exp a (b-1)

let rec  netexp : nat -> nat -> nat
= fun n1 n2 ->
let a = nattoint n1 in
let b= nattoint n2 in
let c= exp a b in (inttonat c)

(*problem3*)

type formula =
	True
	|False
	|Var of string
	|Neg of formula
	|And of formula * formula
	|Or of formula * formula
	|Imply of formula * formula
	|Iff of formula * formula


	

let rec sat : formula -> bool
= fun f ->
match f with
|True -> true
|False -> false
|Var a -> true
|Neg form -> if sat form = true then false else true
|And (f1,f2) -> if (sat f1 && sat f2) || (sat f1 && (sat (Neg f2))||((sat(Neg f1)) && sat f2)||((sat (Neg f1)) && (sat (Neg f2)) )) = true then true else false
|Or (f1, f2) -> if (sat f1 || sat f2) ||(sat f1 || (sat ( Neg f2)) || ((sat(Neg f1)) || sat f2)||((sat(Neg f1)) || (sat (Neg f2)) )) = true then true else false
|Imply(f1, f2) ->if sat f2 = true then sat f1=true else sat f1 = false
|Iff(f1,f2) -> if ((sat f1 = true && sat f2 = true) && (sat f1 = false && sat f2= false))||(( sat (Neg f1) = true && sat f2 = true) && (sat (Neg f1) = false && sat f2= false)) ||((sat f1 = true && sat (Neg f2) = true) && (sat f1 = false && sat (Neg f2)= false)) then true else false






(*problem 4*)

type aexp = 
|Const of int
|Var of string
|Power of string *int
|Times of aexp list
|Sum of aexp list

let rec  diff : aexp * string -> aexp
=fun(e,x) -> 
match e with
|Const a -> Const 0
|Var b -> if x = b then Const 1 else Const 0
|Power (b, c) -> if x=b then Times [Const c; Power(b,(c-1))]  else Const 0
|Times aelist -> 
  (match aelist  with 
  |[]-> Const 0
  |hd::tl -> Sum[(Times[hd;(diff((Times tl),x))]); (Times ((diff(hd,x))::tl))])
|Sum aexplist -> 
  match aexplist with 
  |[]->Const 0
  |hd::tl -> Sum [(diff (hd, x)); (diff((Sum tl), x))]

(*problem 5*)
type exp = X
|INT of int
|ADD of exp * exp
|SUB of exp * exp
|MUL of exp * exp
|DIV of exp * exp
|SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> let rec defineexp (e,x) = 
match e with
|X -> x
|INT a -> a
|ADD (a, b) 
-> defineexp (a,x) + defineexp (b,x)
|SUB (a, b) -> defineexp (a,x) - defineexp (b,x)
|MUL (a, b) -> defineexp (a,x) * defineexp (b,x)
|DIV (a, b) -> defineexp (a,x) / defineexp (b,x)
|SIGMA (a, b, f) -> defineexp ( SIGMA (ADD(a, INT(1)),b,f),x) in defineexp (e,1)

(*problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
			|CompoundBranch of length * mobile
and length = int
and weight = int

let rec value mo =
	match mo with
	|SimpleBranch(l,w) -> l*w
	|CompoundBranch(l,subm) -> 
		(match subm with
		|(b1,b2)->l * (value b1+ value b2 ))


let balanced : mobile -> bool
= fun m -> match m with
|(b1,b2) -> if (value b1) = (value b2) then true else false

(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec chtobin l =
match l with
|[]->[]
|hd::tl -> if hd = ONE then chtobin tl@[1] else chtobin tl@[0]

let rec bintoch l =
match l with
|[]->[]
|hd::tl -> if hd = 1 then bintoch tl@[ONE] else bintoch tl@[ZERO]

let rec bintoint l=
match l with
|[]->0
|hd::tl->hd+(2*bintoint tl)

let rec inttobin a= 
match a with
  |0-> []
  |a-> [a mod 2] @ inttobin (a/2)


let rec bmul : bin->bin->bin
= fun b1 b2 -> 
let a = bintoint(chtobin b1)in
let b = bintoint(chtobin b2) in
let c = (inttobin (a*b)) in bintoch c
