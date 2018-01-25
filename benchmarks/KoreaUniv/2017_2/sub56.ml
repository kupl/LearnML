(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> let rec mir t = match t with | Empty -> Empty | Node(n, a, b) -> Node(n,mir b,mir a) in mir t

(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> let rec na n1 n2 = match n2 with | ZERO -> n1 | SUCC nat -> na (SUCC n1) nat in na n1 n2

let natmul : nat -> nat -> nat 
= fun n1 n2 -> let n = n1 in let rec nm n1 n2 = match n2 with | ZERO -> ZERO | SUCC ZERO -> n1 | SUCC nat -> nm (natadd n1 n) nat in nm n1 n2

let natexp : nat -> nat -> nat 
= fun n1 n2 -> let n = n1 in let rec ne n1 n2 = match n2 with | ZERO -> (SUCC ZERO) | SUCC ZERO -> n1 | SUCC nat -> ne (natmul n1 n) nat in ne n1 n2

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

let sat : formula -> bool
= fun f -> 
let rec funsat x = match x with
| True -> True
| False -> False
| Var string -> if string = "P" then (funsat True) else if string = "Q" then funsat False else raise (Failure "input error")
| Neg f1 -> if (funsat f1 = True) then False else if (funsat f1 = False) then True else raise (Failure "bool error")
| And (f1, f2) -> if (funsat f1 = True && funsat f2 = True) then True else False
| Or (f1, f2) -> if (funsat f1 = True || funsat f2 = True) then True else False
| Imply (f1, f2) -> if (funsat f1 = True) then funsat f2 else True
| Iff (f1, f2) -> if (f1 = f2) then True else False 
in if (funsat f = True) then true else false 

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) ->
let rec df (e,x) = match e with
| Sum(hd::tl) -> Sum([(df (hd,x));(df (Sum(tl),x))])
| Sum(_)-> Const(0)
| Times(hd::tl) -> if hd = Var(x) then Times(tl@[Const 1]) else Times([hd;(df (Times(tl),x))])
| Times(_)-> Times([Const 0])
| Power(s,i) -> if s = x then Times([Const(i);Var(s)]) else Const(0);
| Const(i) -> Const 0
| Var(s) -> if s = x then Const(1) else Const(0)
in df(e,x)

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> let rec f e x = match e with
	| X -> x 
	| (INT n) -> n 
	| ADD (n,m) -> (f n x) + (f m x) 
	| SUB (n,m) -> (f n x) - (f m x) 
	| MUL (n,m) -> (f n x) * (f m x)
	| DIV (n,m) -> if (f m x) = 0 then raise (Failure "zero divide error") else (f n x)/(f m x)
	| SIGMA (n,m,k) -> if ((f n x) <= (f m x)) then  (f k (f n x)) + (f (SIGMA (ADD(INT 1, n),m,k)) x)  else 0
	in f e 0

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> 
let rec wb b = match b with 
	| SimpleBranch(q,w) -> w
	| CompoundBranch(q,w) ->
		match w with 
		| (z,x) -> ((wb z)+(wb x))
		in let rec bals m = match m with
			| SimpleBranch(q,w) -> q*(wb m)
			| CompoundBranch(q,w) -> q*(wb m)
			in let rec bal m = match m with
				| SimpleBranch(a,b),SimpleBranch(c,d) -> if (bals (SimpleBranch(a,b)) = bals (SimpleBranch(c,d))) then true else false
				| CompoundBranch(a,b),SimpleBranch(c,d) -> if (bal b) then (if (bals (CompoundBranch(a,b)) = bals (SimpleBranch(c,d))) then true else false) else false
				| SimpleBranch(a,b),CompoundBranch(c,d) -> if (bal d) then (if (bals (SimpleBranch(a,b)) = bals (CompoundBranch(c,d))) then true else false) else false
				| CompoundBranch(a,b),CompoundBranch(c,d) -> if (bal b && bal d) then (if (bals (CompoundBranch(a,b)) = bals (CompoundBranch(c,d))) then true else false) else false
				in bal m


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> 
	let rec rvs bl = match bl with 
	| [] -> []
	| hd::tl -> (rvs tl)@[hd]
	in let tails a = match a with | [] -> [] | hd::tl -> tl
	in let head a = match a with | [] -> [] | hd::tl -> [hd]
	in let rec op a1 a2 = match a2 with
	| [] -> [a1]
	| ONE::tl -> if(a1 = ONE) then ZERO::(op ONE tl) else a2
	| ZERO::tl -> a1::tl
	in let rec ops a3 a4 = match a3 with
	| [] -> a4
	| hd::tl -> head (op hd (a4))@(ops tl (tails (op hd (a4))))
	in let rec mb a1 a4 k = 
	match a1 with 
	| [] -> k
	| hd::tl -> if(hd = ZERO) then mb tl (ZERO::a4) k 
				else mb tl (ZERO::a4) ((a4)::k)
	in let rec mub k = match k with | []->[] | hd::tl -> (ops hd (mub tl))
	in let rec xzero j = match j with | [] -> [ZERO] | hd::tl -> if(hd = ONE) then j else xzero tl
	in let rec a b1 b2 = mub (mb (rvs b1) (rvs b2) []) 
	in xzero ((rvs (a b1 b2)))