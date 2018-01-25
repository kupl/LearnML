(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> (* TODO *)
	let rec sub u =
		match u with
		| Empty -> Empty
		| Node (d,l,r) -> Node(d,sub r,sub l)
	in sub t

(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
	let rec sub a b =
		match a with
		| ZERO -> b
		| SUCC c -> SUCC(sub c b) 
	in sub n1 n2

let natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
	let rec sub a b ans =
		match a with
		| ZERO -> ans
		| SUCC c -> sub c b (natadd ans b)
	in sub n1 n2 ZERO 

let natexp : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
	let rec sub a b ans =
		match b with
		| ZERO -> ans
		| SUCC c -> sub a c (natmul ans a)
	in sub n1 n2 (SUCC ZERO)

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
	let comp a b =
		if a>b then 1 else 0
	in
	let rec findenv f =
		match f with
		| Var s -> [s]
		| Neg a -> findenv a
		| And (a,b) -> List.append (findenv a) (findenv b)
		| Or (a,b) -> List.append (findenv a) (findenv b)
		| Imply (a,b) -> List.append (findenv a) (findenv b)
		| Iff (a,b) -> List.append (findenv a) (findenv b)
		| _ -> []
	in
	let rec rdcenv al =
		match al with
		| [] -> []
		| hd::tl ->
			match tl with
			| [] -> [hd]
			| h2::t2 -> if hd=h2 then (rdcenv tl) else hd::(rdcenv tl)
	in
	let rec matenv f al =
		match f with
		| True -> true
		| False -> false
		| Var s -> 
			let rec matdat al =
				match al with
				| [] -> false
				| hd::tl ->		
					 match hd with (a,b) -> if a=s then b else matdat tl
			in matdat al 
		| Neg a -> not(matenv a al)
		| And (a,b) -> (matenv a al)&&(matenv b al)
		| Or (a,b) -> (matenv a al)||(matenv b al)
		| Imply (a,b) -> matenv (Or(Neg a, And(a,b))) al
		| Iff (a,b) -> matenv (Or(And(Neg a,Neg b), And(a,b))) al
	in
	let rec crtenv al =
		match al with
		| [] -> [[]]
		| hd::tl -> let next = crtenv tl in List.append (
			List.map (fun x -> (hd,true)::x) next) (
			List.map (fun x -> (hd,false)::x) next)
	in
	let env = List.sort comp (findenv f) in
	let env = rdcenv env in
	let e = crtenv env in
	let rec inc al =
		match al with
		| [] -> false
		| hd::tl -> (matenv f hd)||(inc tl)
	in
		inc e

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> (* TODO *)
	let rec subdiff e x =
	match e with
	| Const _ -> Const 0
	| Var s -> if s=x then Const 1 else Const 0
	| Power (s,n) -> if s=x then Times[Const n;Power (s,n-1)] else Const 0
	| Times al ->
		let rec sub b =
			match b with
			| [] -> Const 0
			| hd::tl -> Sum[Times ((subdiff hd x)::tl); Times[hd;(sub tl)]]
		in sub al
	| Sum al ->
		let rec sub b =
			match b with
			| [] -> []
			| hd::tl -> (subdiff hd x)::(sub tl)
		in Sum (sub al)
	in subdiff e x

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> (* TODO *) 
	let rec calc e v =
		match e with
		| X -> v
		| INT (i) -> i
		| ADD (e1,e2) -> calc e1 v + calc e2 v
		| SUB (e1,e2) -> calc e1 v - calc e2 v
		| MUL (e1,e2) -> calc e1 v * calc e2 v
		| DIV (e1,e2) -> calc e1 v / calc e2 v
		| SIGMA (e1,e2,e3) ->
			let i1 = calc e1 v in
			let i2 = calc e2 v in
			if i1>i2 then 0 
			else if i1=i2 then calc e3 i1
			else calc e3 i1 + calc (SIGMA(INT (i1+1),INT i2,e3)) v
	in
		calc e 0

(* problem 6*)
type mobile = branch * branch     (* left and right branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> (* TODO *)
	let rec force b =
		match b with
		| SimpleBranch (_,w) -> w
		| CompoundBranch(_,m) ->
			match m with (b1,b2) ->
				force b1 + force b2
	in
	let netf m =
		match m with (b1,b2) -> force b1 + force b2
	in
	let torque b = 	(*Clockwise is negative:Abondoned*)
		match b with
		| SimpleBranch (l,w) -> l*w
		| CompoundBranch (l,m) -> l*netf m
	in
	let chktqe m =
		match m with (b1,b2) ->
			if torque b1 = torque b2 then true else false
	in
	let rec subbal m =
		let a = 
			match m with (b1,_) ->
			match b1 with
			| SimpleBranch (_,_) -> true
			| CompoundBranch (_,mb) -> subbal mb
		in
		let b =
			match m with (_,b2) ->
			match b2 with
			| SimpleBranch (_,_) -> true
			| CompoundBranch (_,mb) -> subbal mb
		in
		let c =
			chktqe m
		in
			a&&b&&c
	in
		subbal m

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
	let bnot d = if d=ZERO then ONE else ZERO in
	let fulladd d1 d2 d3 =
		if d1=ZERO then
			if d2=ZERO then (ZERO,d3) else (d3,bnot d3)
		else if d2=ZERO then (d3,bnot d3) else (ONE,d3)
	in
	let rec adder b1 b2 cr =
		match b1 with
		| [] -> (
			match b2 with
			| [] -> if cr=ONE then [ONE] else []
			| h2::t2 ->
				match fulladd ZERO h2 cr with (a,b) ->
					b::(adder b1 t2 a))
		| h1::t1 ->
			match b2 with
			| [] -> (match fulladd ZERO h1 cr with
				(a,b) -> b::(adder t1 b2 a))
			| h2::t2 ->
				match fulladd h1 h2 cr with
				(a,b) -> b::(adder t1 t2 a)
	in
	let add b1 b2 = List.rev (adder (List.rev b1) (List.rev b2) ZERO) in
	let rec mul b1 b2 ans =
		match b2 with
		| [] -> ans
		| hd::tl -> 
			let newans = add ans ans in
			if hd=ZERO then mul b1 tl newans
			else mul b1 tl (add b1 newans)
	in
		mul b1 b2 [ZERO]