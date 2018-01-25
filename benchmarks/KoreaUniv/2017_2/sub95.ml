(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
	Empty -> Empty
	|Node(n, left, right) -> Node(n, mirror right, mirror left);;


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	ZERO -> n2
	|SUCC(succ) -> SUCC(natadd succ n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	ZERO -> ZERO
	|SUCC(succ) -> natadd (natmul succ n2) n2;;

let rec natexp : nat -> nat -> nat
= fun n1 n2 -> match n2 with
	ZERO -> SUCC ZERO
	|SUCC(succ) -> natmul n1 (natexp n1 succ);;


(* problem 3*)
type formula =
	True
 |False
 |Var of string
 |Neg of formula
 |And of formula * formula
 |Or of formula * formula
 |Imply of formula * formula
 |Iff of formula * formula

let rec satstr : formula -> formula
= fun g -> match g with
	True -> True
 |False -> False
 |Var s -> Var s
 |Neg (Neg (g1)) -> satstr g1
 |Neg (g1) -> if (satstr g1)=True then False else if (satstr g1)=False then True else Neg (satstr g1)
 |And (g1,g2) ->
		if (satstr g1)=(satstr (Neg (g2))) then False
		else if (satstr g1)=True && (satstr g2)=True then True
		else if (satstr g1)=False && (satstr g2)=False then False
		else True
 |Or (g1,g2) -> if (satstr g1)=False && (satstr g2)=False then False else True
 |Imply (g1, g2) -> if (satstr g1)=True && (satstr g2)=False then False else True
 |Iff (g1, g2) -> if (satstr g1)=(satstr g2) then True else False;;
		
let rec sat : formula -> bool
= fun f -> match f with
	True -> true
 |False -> false
 |Var (s) -> true
 |_ -> sat (satstr f);;


(*problem 4*)
type aexp =
	|Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
	Const n -> Const 0
	|Var x -> Const 1
	|Power (x,n) ->
		(match n with
			0 -> Const 0
			|1 -> Const 1
			|_ -> Times[Const n; Power (x, n-1)])
	|Times (hd::tl) ->
		(match hd with
			|Const n -> if n=0 then Const 0 else (match tl with [] -> Const 0 |_ -> Times[Const n; diff(Times tl, x)])
			|Var x ->
				(match tl with
					[] -> Const 1
					|_ -> Times[Const 1; diff(Times tl, x)])
			|Power (x,n) ->
				(match tl with
					[] -> diff(Power(x,n),x)
					|_ -> Times[Times[Const n; Power (x,n-1)]; diff(Times tl,x)])
			|_ -> Times [diff(hd,x);diff(Times tl,x)])
	|Sum (hd::tl) ->
		(match hd with
		|Const n -> (match tl with []->Const 0 |_->diff(Sum tl,x))
		|_ -> Sum[diff (hd,x);diff (Sum tl,x)])
	|_ -> Const 0;; 


(*problem 5*)
type exp = X
					|INT of int
					|ADD of exp * exp
					|SUB of exp * exp
					|MUL of exp * exp
					|DIV of exp * exp
					|SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> match e with
	|X -> calculator X
	|INT n -> n
	|ADD (e1,e2) -> ((calculator e1) + (calculator e2))
	|SUB (e1,e2) -> ((calculator e1) - (calculator e2))
	|MUL (e1,e2) -> ((calculator e1) * (calculator e2))
	|DIV (e1,e2) -> ((calculator e1) / (calculator e2))
	|SIGMA (e1,e2,e3) ->
		(match e1, e2, e3 with
		|(INT n1), (INT n2), _ ->
			if (n1<=n2) then
				(match e3 with
				|X -> n1 + calculator (SIGMA (INT (n1+1),e2,e3))
				|INT n -> n + calculator (SIGMA (INT (n1+1),e2,e3))
				|ADD (t1,t2) -> calculator (SIGMA (e1,e2,t1)) + calculator (SIGMA (e1,e2,t2)) + calculator (SIGMA (INT(n1+1),e2,e3))
				|SUB (t1,t2) -> calculator (SIGMA (e1,e2,t1)) - calculator (SIGMA (e1,e2,t2)) + calculator (SIGMA (INT(n1+1),e2,e3))
				|MUL (t1,t2) -> calculator (SIGMA (e1,e1,t1)) * calculator (SIGMA (e1,e1,t2)) + calculator (SIGMA (INT(n1+1),e2,e3))
				|DIV (t1,t2) -> calculator (SIGMA (e1,e1,t1)) / calculator (SIGMA (e1,e1,t2)) + calculator (SIGMA (INT(n1+1),e2,e3))
				|_ -> raise (Failure ("Error")))
			else 0
		|_ -> raise (Failure ("Error2")));;


(*problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
						|CompoundBranch of length * mobile
and length = int
and weight = int

let rec weight : mobile -> int
= fun m -> match m with
	(SimpleBranch (l1,w1),SimpleBranch (l2,w2)) -> w1+w2
	|(SimpleBranch (l1,w1),CompoundBranch (l2,m1)) -> w1+weight(m1)
	|(CompoundBranch (l1,m1),SimpleBranch (l2,w2)) -> weight(m1)+w2
	|(CompoundBranch (l1,m1),CompoundBranch(l2,m2)) -> weight(m1)+weight(m2);;

let rec balanced : mobile -> bool
= fun m -> match m with
	(SimpleBranch (l1,w1),SimpleBranch (l2,w2)) -> if (l1*w1=l2*w2) then true else false
	|(SimpleBranch (l1,w1),CompoundBranch (l2,m1)) -> if (l1*w1=l2*(weight(m1))) then true else false
	|(CompoundBranch (l1,m1),SimpleBranch (l2,w2)) -> if (l1*(weight(m1))=l2*w2) then true else false
	|(CompoundBranch (l1,m1),CompoundBranch (l2,m2)) -> if (l1*(weight(m1))=l2*(weight(m2))) then true else false;;
			

(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bintoint : bin -> int -> int
= fun b a -> match b with
	[] -> a
	|[ZERO] -> a*2
	|[ONE] -> 2*a+1
	|hd::tl -> (match hd with |ZERO -> (bintoint tl (a*2)) |ONE -> (bintoint tl (a*2+1)));;

let rec inttobin : int -> bin -> bin
= fun b lst-> match b with
	|0 -> ZERO::lst
	|1 -> ONE::lst
	|_ -> if (b mod 2)=0 then (inttobin (b/2) (ZERO::lst)) else (inttobin ((b-1)/2) (ONE::lst));;

let bmul : bin -> bin -> bin
= fun b1 b2 -> inttobin ((bintoint b1 0) * (bintoint b2 0)) [];;
