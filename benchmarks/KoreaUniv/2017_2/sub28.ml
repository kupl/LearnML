exception Problem

(*problem1*)

type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun tree ->
	match tree with 

	| Node(i,t1, t2) -> Node (i, mirror t2, mirror t1)
	| Empty -> Empty


	(*proble2*)

	type nat = ZERO | SUCC of nat

	let rec natadd : nat -> nat -> nat
	= fun n1 n2 -> 
	match n2 with
	| ZERO -> n1
	| SUCC n -> natadd (SUCC n1) n

	let rec natmul : nat -> nat -> nat
	= fun n1 n2 -> 
	match n2 with
	| ZERO -> ZERO
	| SUCC n -> natadd n1 (natmul n1 n)
	
	let rec natexp : nat -> nat -> nat
	= fun n1 n2 -> 
	match n2 with
	| ZERO -> SUCC ZERO
	| SUCC n -> natmul n2 (natexp n1 n)





(*problem3*)
type formula =
	TRUE
	| FALSE
	| Var of string
	| Neg of formula
	| And of formula * formula
	| Or of formula * formula
	| Imply of formula * formula
	| Iff of formula * formula

let trans_tr : string -> bool
= fun str->  
match str with 
|_ ->true


let trans_fl : string -> bool 
= fun str->  
match str with 
|_ -> false


let rec decide : formula -> bool
= fun f ->
match f with
| TRUE -> true
| FALSE -> false
| Var str ->  
		trans_tr str
| Neg f -> not (decide f)
| And (f1,f2) ->(((decide f1)&&(decide f2)) || ((decide (Neg f1))&&(decide f2) ) || ((decide f1)&& (decide (Neg f2))) || ((decide (Neg f1))&&(decide (Neg f2))))
| Or (f1,f2) -> (((decide f1)||(decide f2)) || ((decide (Neg f1))||(decide f2))  || ((decide f1)||(decide (Neg f2))) || ((decide (Neg f1))||(decide (Neg f2))) )
| Imply (f1,f2)-> if (decide f1) = false then true else (decide f2) 
| Iff (f1,f2)-> if ((decide f1) = (decide f2)) then true else false 

let rec sat :formula-> bool
= fun f->
 if (decide f = false) then false
else true




(*problem4*)

type aexp =
	|Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> 
 match e with
 | Const _ -> Const 0
 | Var y -> if x=y then Const 1 else Const 0
 | Power (y,n) -> if x=y then Times [Const n; Power (y, n-1)] else Const 0
 | Times l -> 
 	(match l with
 	| [] -> raise Problem
 	| [a] -> raise Problem
 	| [a1;a2] -> Sum [Times [diff (a1,x); a2] ; Times [a1; diff (a2,x)]] 
 	| hd :: tl -> Sum [Times [diff (hd,x); Times tl]; Times [hd; diff(Times tl,x)]])
 | Sum l ->
 	(match l with
 	| [] -> raise Problem
 	| [a] -> raise Problem
 	| [a1;a2] -> Sum [diff(a1,x); diff(a2,x)]
 	| hd::tl -> Sum [diff(hd,x); diff(Sum tl,x)])





(*problem5*)

type exp = 
	| X 
	| INT of int 
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp


let rec calculator : exp -> int
= fun e -> 
match e with
	| X -> raise Problem
	| INT n -> n
	| ADD (e1,e2) -> calculator e1 + calculator e2
	| SUB (e1,e2) -> calculator e1 - calculator e2
	| MUL (e1,e2) -> calculator e1 * calculator e2
	| DIV (e1,e2) -> calculator e1 / calculator e2
	| SIGMA (e1,e2,e3) -> 
	let p,q = calculator e1, calculator e2 in
		if (p < q) 
		then (sigma_sub e3 p + calculator (SIGMA (INT(p +1), INT q, e3)))
		else if p = q then sigma_sub e3 p 
		else raise Problem

and sigma_sub : exp -> int -> int
= fun e n ->
match e with
	| X -> n
	| INT k -> k
	| ADD (e1,e2) -> sigma_sub e1 n + sigma_sub e2 n
	| SUB (e1,e2) -> sigma_sub e1 n - sigma_sub e2 n
	| MUL (e1,e2) -> sigma_sub e1 n * sigma_sub e2 n
	| DIV (e1,e2) -> sigma_sub e1 n / sigma_sub e2 n
	| SIGMA (e1,e2,e3) -> calculator e


(*problem6*)

type mobile = branch * branch (*left and right branches *)
and branch = SimpleBranch of length * weight
			| CompuondBranch of length * mobile
and length = int
and weight = int
let rec weight_mb : mobile -> int
= fun m ->
match m with 
	|SimpleBranch(ll,lw), SimpleBranch(rl,rw) -> lw+rw 
	|SimpleBranch(ll,lw), CompuondBranch(rl,cm) -> lw+weight_mb cm 
	|CompuondBranch(ll,cm), SimpleBranch(rl,rw) -> rw+weight_mb cm
	|CompuondBranch(ll,lm),CompuondBranch(rl,rm) ->weight_mb lm+weight_mb rm



let rec balanced : mobile -> bool
= fun m -> 
match m with 
	|SimpleBranch (ll, lw) , SimpleBranch(rl, rw) -> (ll*lw=rl*rw)
	|SimpleBranch (ll, lw) , CompuondBranch (rl, cm) -> (ll*lw=rl*(weight_mb cm)&& balanced cm)
	|CompuondBranch(ll,cm) , SimpleBranch(rl,rw) -> (ll*(weight_mb cm)=rl*rw&&balanced cm)
	|CompuondBranch(ll,lcw), CompuondBranch(rl,rcw)->(ll*(weight_mb lcw)=rl*(weight_mb rcw)&&balanced lcw &&balanced rcw)




(*problem7*)

type digit = ZERO | ONE
type bin = digit list


let rec expo : int -> int -> int
= fun p q ->
if q>0 then p*expo p (q-1)
else if q=0 then 1
else raise Problem

let rec list_length : 'a list -> int
= fun l ->
match l with
|[] -> 0
|hd::tl -> 1 + list_length tl


let rec convert_deci : bin -> int
= fun b -> 
match b with
|[]-> 0
|hd::tl -> 
			if hd= ONE then (expo 2 (list_length tl) + convert_deci tl)
			else convert_deci tl

let rec convert_bin : int -> bin
=fun i ->
if (i /2) >= 1 && i mod 2 =1 then 	convert_bin (i/2) @[ONE]
else if(i/2)>=1 && i mod 2 =0 then convert_bin(i/2) @[ZERO]
else if i/2 =1/2 then [ONE]
else raise Problem

let bmul : bin -> bin -> bin
= fun b1 b2 ->
convert_bin (convert_deci b1 * convert_deci b2)
























