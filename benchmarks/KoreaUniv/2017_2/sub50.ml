(* problem 1 *)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
			|Empty -> Empty
			|Node(i,a,b) -> Node(i, mirror b, mirror a)

(*problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
				|ZERO -> n2
				|SUCC n -> SUCC (natadd n n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
				|ZERO -> ZERO
				|SUCC n -> natadd n2 (natmul n n2)

let rec natexp : nat -> nat -> nat
= fun n1 n2 -> match n2 with
				|ZERO -> SUCC ZERO
				|SUCC n -> natmul n1 (natexp n1 n)

(*problem 3*)
type formula =
	True
	|False
	|Var of string
	|Neg of formula
	|And of formula * formula
	|Or of formula * formula
	|Imply of formula * formula
	|Iff of formula * formula
(*
type env = (var * formula) list
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env x e =
	match e with
	|[] -> raise (Failure ("variable" ^ x ^ " not found"))
	|(y,v)::tl -> if x = y then v else apply_env x tl

let rec eval : exp -> env -> bool
= fun exp env ->
	match exp with
	|True -> true
	|False -> false
	|Var x -> if x = true then true else false
	|Neg x -> if x = true then false else true
	|And (e1,e2) -> if sat e1 = sat e2 then true else false
	|Or (e1,e2) ->  if sat e1 = sat e2 then false else true
	|Imply (e1,e2) ->  if sat e1 = false && sat e2 = false then true else false
	|Iff (e1,e2) ->  Or (e1,e2)

let tc list = [(true,true);(true,false);(false,true);(false,false)]
*)
let rec sat : formula -> bool
= fun f -> match f with
			|True -> true
			|False -> false
			|Var x -> true
			|Neg x -> true
			|And (e1,e2) -> if sat e1 = sat e2 then true else false
			|Or (e1,e2) -> if sat e1 = sat e2 then false else true 
			|Imply (e1,e2) -> if sat e1 = true && sat e2 = false then true else false
			|Iff (e1,e2) -> if sat e1 = sat e2 then false else true 

(*problem 4*)
type aexp =
	|Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
				|Const c -> Const 0
				|Var v-> if v = x then Const 1 else Const 0
				|Power (v,c) -> if v = x then Times [Const c; Power (v, (c-1))] else Const 0
				|Times [] -> Const 0
				|Times (hd::tl) -> if tl = [] then diff(hd,x)
								else Sum [Times (diff (hd, x):: tl); Times [hd; diff (Times tl, x)]]
				|Sum [] -> Const 0
				|Sum (hd::tl) -> if tl = [] then diff(hd,x)
								else Sum [diff (hd, x); diff(Sum tl, x)]

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
			|X -> raise(Failure "type error")
			|INT a -> a
			|ADD (a,b) -> calculator a + calculator b
			|SUB (a,b) -> calculator a - calculator b
			|MUL (a,b) -> calculator a * calculator b
			|DIV (a,b) -> calculator a / calculator b
			|SIGMA (st,ls,x) -> let a = calculator st in 
								let b = calculator ls in
								if a = b then
									let rec sigc = fun idx ex -> match ex with
									|X -> sigc idx idx
									|INT n -> n
									|ADD (j,k) -> (sigc idx j) + (sigc idx k)
									|SUB (j,k) -> (sigc idx j) - (sigc idx k)
									|MUL (j,k) -> (sigc idx j) * (sigc idx k)
									|DIV (j,k) -> (sigc idx j) / (sigc idx k)
									|SIGMA (s,l,pol) -> calculator e
									in sigc ls x
							else (calculator (SIGMA (st, st, x)) + calculator (SIGMA (ADD(st,INT 1),ls,x)))


(*problem 6*)
type mobile = branch * branch (*left and right and branches*)
and branch = SimpleBranch of length * weight | CompoundBranch of length * mobile
and length = int
and weight = int

let rec bweight
= fun b -> match b with
			|SimpleBranch(l,w) -> w
			|CompoundBranch(l, (b1,b2)) -> ((bweight b1) + (bweight b2))

let omega
= fun m -> match m with
			|SimpleBranch(l,w) -> l*w
			|CompoundBranch(l,(b1,b2)) -> l * ((bweight b1)+(bweight b2))

let rec balanced : mobile -> bool
= fun m -> match m with
			|(SimpleBranch (l1,w1), SimpleBranch(l2,w2))
				-> if l1 * w1 = l2 * w2 then true else false
			|(CompoundBranch(l1, (b1,b2)),SimpleBranch(l2,w1))
				-> let wl = omega (CompoundBranch(l1,(b1,b2))) in
					let wr = omega (SimpleBranch(l2,w1)) in
					if ((balanced (b1,b2) = true) && wl = wr)
					then true else false
			|(SimpleBranch(l1,w1),CompoundBranch (l2,(b1,b2)))
				-> let wl = omega (SimpleBranch(l1,w1)) in
					let wr = omega (CompoundBranch(l2,(b1,b2))) in
					if ((balanced (b1,b2) = true) && (wl = wr))
					then true else false
			|(CompoundBranch(l1,(b1,b2)),CompoundBranch(l2,(b3,b4)))
				-> let wl = omega (CompoundBranch(l1,(b1,b2))) in
					let wr = omega (CompoundBranch(l2,(b3,b4))) in
					if balanced (b1,b2) = true
					&& balanced (b3,b4) = true
					&& wl = wr
					then true else false


(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec lislen : bin -> int
= fun a -> match a with
			|[] -> 0
			|hd::tl -> 1+(lislen tl)

let rec pow : (int * int) -> int
= fun (a, b) -> match b with
				|0 -> 1
				|1 -> a
				|i -> pow(a,(i-1))*a

let rec biToDec : bin -> int
= fun a ->match a with
			|[] -> 0
			|hd::tl -> if hd = ZERO then (biToDec tl)
						else if hd = ONE then pow(2,(lislen tl))+(biToDec tl)
						else raise (Failure "input error")

let rec decToBi : int -> bin
= fun n ->if n = 0 then [ZERO]
			else if n = 1 then [ONE]
			else if n mod 2 = 0 then decToBi(n/2)@[ZERO]
			else if n mod 2 = 1 then decToBi(n/2)@[ONE]
			else raise (Failure "input error")

let rec bmul : bin -> bin -> bin
= fun b1 b2 -> let x = (biToDec b1) * (biToDec b2) in (decToBi x)