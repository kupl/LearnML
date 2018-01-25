(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> 
	match t with
	| Empty -> Empty
	| Node (a,t1,t2) ->Node (a,mirror t2,mirror t1);;
	
(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
	match (n1,n2) with
	| (n1,ZERO) -> n1
	| (ZERO,n2) -> n2
	| (SUCC (n3), SUCC (n4)) -> natadd (SUCC (SUCC (n3))) n4;;


let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
	match (n1,n2) with
	| (n1,ZERO) -> ZERO
	| (ZERO,n2) -> ZERO
	| (n1, SUCC ZERO) -> n1
	| (SUCC ZERO, n2) -> n2
	| (SUCC (n3), SUCC (n4)) -> natadd (natmul (SUCC (n3)) (n4)) (SUCC (n3));;
 

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> 
	match (n1,n2) with
	| (n1, ZERO) -> SUCC ZERO
	| (n1, SUCC ZERO) -> n1
	| (n1, SUCC (n3)) -> natmul (natexp n1 n3) n1;;


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



let rec sat : formula -> bool
= fun f -> 
match f with 
| True -> true
| False -> false
| Var (s) -> true
| Neg (f) -> if (sat f) then false else true
| And (f1,f2) -> (sat f1)&&(sat f2)
| Or (f1,f2) -> (sat f1)||(sat f2)
| Imply (f1,f2) -> if (sat f1) then true else (sat f2)
| Iff (f1,f2) -> (sat f1) = (sat f2)				
 
(*problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
	
let rec diff : aexp * string -> aexp
= fun (e,x) -> 
	match e with
	| Const n -> Const 0
	| Var x1 -> if x1=x then Const 1 else Const 0
	| Power (x1,n) -> if x1=x then if n=1 then Const 1 else Times [Const n; Power (x,n-1)]
										else Const 0
	| Sum l -> (match l with
						 | [] -> Const 0
						 | hd::tl -> Sum [diff (hd,x); diff ((Sum tl),x)]) 
  | Times l -> match l with
						 | [] -> Const 0
						 | hd::tl -> Sum[(Times ((diff (hd,x))::tl)); (Times [hd;(diff ((Times tl),x))])]						

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> 
	match e with
	| X -> calculator (INT 0)
	| INT n -> n
	| ADD (INT n1, INT n2) -> (n1+n2)
	| ADD (e1,e2) -> ((calculator e1)+(calculator e2))
	| SUB (INT n1, INT n2) -> (n1-n2)
	| SUB (e1, e2) -> ((calculator e1)-(calculator e2))
	| MUL (INT n1, INT n2) -> (n1*n2)
	| MUL (e1, e2) -> ((calculator e1)*(calculator e2))
	| DIV (INT n1, INT n2) -> (n1/n2)
	| DIV (e1, e2) -> ((calculator e1)/(calculator e2))
	| SIGMA (e1, e2, e3) -> let rec sigma : exp-> exp -> int
													=fun e3 e1 -> 
													match e3 with
													| X -> calculator e1
													| INT n -> n
													| ADD (a,b) -> (sigma a e1) + (sigma b e1)
  												| SUB (a,b) -> (sigma a e1) - (sigma b e1)
  												| MUL (a,b) -> (sigma a e1) * (sigma b e1)
												  | DIV (a,b) -> (sigma a e1) / (sigma b e1)
												  | SIGMA (a,b,c) -> calculator (SIGMA (a,b,c))
													in if ((calculator e1)=(calculator e2)) then (sigma e3 e1) 
													else (calculator (SIGMA (ADD (e1, INT 1), e2, e3)))+(sigma e3 e1)
	

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> 
	let rec mass : branch -> int
	= fun cb ->
		match cb with 
		| SimpleBranch (ll,ww) -> ww
		| CompoundBranch (ll,mm) -> match mm with
																| (bb1, bb2) -> (mass bb1)+(mass bb2)
	in let rec torque : branch -> int 
	= fun b ->
		match b with 
		| SimpleBranch (l,w) -> l*w
		| CompoundBranch (l,m) -> match m with
															| (b1, b2) -> l*((mass b1)+(mass b2))
	in match m with
	| (b1,b2) -> match (b1,b2) with
							| (SimpleBranch (_,_),SimpleBranch (_,_)) -> if (torque b1)=(torque b2) then true else false
							| (CompoundBranch (_,m1),SimpleBranch (_,_)) -> if (balanced m1)&&((torque b1)=(torque b2)) then true else false
							| (SimpleBranch (_,_),CompoundBranch (_,m2)) -> if (balanced m2)&&((torque b1)=(torque b2)) then true else false
							| (CompoundBranch (_,m1), CompoundBranch (_,m2)) -> if ((balanced m1)&&(balanced m2))&&((torque b1)=(torque b2)) then true else false
 
(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> 
let rec dtob : int -> bin
=fun n -> 
	let a = n/2 in
		let b = n mod 2 in
			if a = 0 then [ONE]
				else if b=0 then ((dtob a) @ [ZERO])
							else ((dtob a) @ [ONE])
in let rec num : bin -> int
=fun b -> 
	match b with 
	| [] -> 0
	| hd::tl -> 1 + (num (tl))
in let rec btod : bin -> int -> int
= fun b n-> 
	let rec binexp : int -> int
	= fun a -> 
	if a =0 then 1 else 2*(binexp (a-1)) 
	in match b with
	| [] -> 0 
	| hd::tl -> if hd = ONE then binexp(n-1)+(btod tl (n-1)) else btod tl (n-1)
in dtob((btod b1 (num b1))*(btod b2 (num b2)))
