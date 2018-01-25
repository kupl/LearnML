(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
		| Empty -> t
		| Node(int, x, y) -> Node(int, mirror y, mirror x)


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
		| SUCC(x) -> SUCC(natadd x n2)
		| ZERO -> match n2 with
				| SUCC(y) -> SUCC(natadd ZERO y)
				| ZERO -> ZERO

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
		| SUCC(x) -> (natadd (natmul n1 x) n1)
		| ZERO -> ZERO

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
		| SUCC(x) -> (natmul (natexp n1 x) n1)
		| ZERO -> SUCC(ZERO)



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

let rec getEnv : 'a list -> string -> bool
= fun env x -> match env with
			| [] -> true
			| (k, v)::tl -> if k=x then v else (getEnv tl x)

let rec calc : formula -> 'a list -> bool
= fun f e -> match f with
		| True -> true
		| False -> false
		| Var(x) -> (getEnv e x)
		| Neg(x) -> let res_x = (calc x e) in
			    if res_x then false else true
		| And(x, y) -> let res_x = (calc x e) in
			       let res_y = (calc y e) in
			       if res_x then if res_y then true else false else false
		| Or(x, y) -> let res_x = (calc x e) in
			      let res_y = (calc y e) in
			      if res_x then true else if res_y then true else false
		| Imply(x, y) -> let res_x = (calc x e) in
				 let res_y = (calc y e) in
					if res_x then if res_y then true else false else true
		| Iff(x, y) -> let res_x = (calc x e) in
			       let res_y = (calc y e) in
					if res_x=res_y then true else false

let rec process : formula -> string list -> 'a list -> bool
= fun f env nenv-> match env with
			| [] -> (calc f nenv)
			| hd::tl -> if (process f tl ((hd, true)::nenv)) then true
					else if (process f tl ((hd, false)::nenv)) then true
					     else false

let rec makeEnv : formula -> 'a list
= fun f -> match f with
		| True -> []
		| False -> []
		| Var(x) -> [(x,false)]
		| Neg(x) -> (makeEnv x)
		| And(x, y) -> (makeEnv x)@(makeEnv y)
		| Or(x, y) -> (makeEnv x)@(makeEnv y)
		| Imply(x, y) -> (makeEnv x)@(makeEnv y)
		| Iff(x, y) -> (makeEnv x)@(makeEnv y)

let rec makeSet : 'a list -> string list
= fun l -> match l with
		| [] -> []
		| (hd, _)::tl -> if (getEnv tl hd)=false then (makeSet tl) else hd::(makeSet tl)

let sat : formula -> bool
= fun f -> let env = (makeEnv f) in (process f (makeSet env) [])



(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
		| Const(i) -> (Const (0))
		| Var(s) -> if s=x then (Const (1)) else (Const(0))
		| Power(s, i) -> if s=x then Times([Const(i); Power(s, (i-1))]) else (Const(0))
		| Times(l) -> let rec difft : (aexp list) -> (aexp list)
			      = fun l -> (match l with
						| []->[]
						| hd::tl->if diff(hd, x)=Const(0) then hd::(difft tl) else diff(hd, x)::(difft tl))
			      in Times(difft l)
		| Sum(l) -> let rec diffs : (aexp list) -> (aexp list)
			    = fun l -> (match l with
						| []-> []
						| hd::tl->diff(hd, x)::(diffs tl))
			    in Sum(diffs l)

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calc : exp-> (int list) -> int
= fun exp env -> match exp with
			| X -> (List.hd env)
			| INT(x) -> x
			| ADD(x, y) -> (calc x env) + (calc y env)
			| SUB(x, y) -> (calc x env) - (calc y env)
			| MUL(x, y) -> (calc x env) * (calc y env)
			| DIV(x, y) -> (calc x env) / (calc y env)
			| SIGMA(x, y, z) -> let x = (calc x env) in
					    let y = (calc y env) in
					    let rec sigm : int->int->exp->(int list)->int
					    = fun x y z env -> if x<=y then (calc z (x::env)) + (sigm (x+1) y z env) else 0 in (sigm x y z env)

let calculator : exp -> int
= fun e -> (calc e [])
 
(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec getWeight : mobile -> int
= fun m -> match m with
		| (l, r) -> (match l with
				| SimpleBranch(l, w) -> let left=w in let lw=l*left in
					(match r with
						| SimpleBranch(l, w) -> let rw = l*w in
							if lw=rw then left+w else -1
						| CompoundBranch(l, m) -> let right=(getWeight m) in let rw = l*right in if rw<0 then -1 else if lw=rw then left+right else -1)
				| CompoundBranch(l, m) -> let left=(getWeight m) in let lw = l*left in if lw<0 then -1 else
					(match r with
						| SimpleBranch(l, w) -> let rw = l*w in
							if lw=rw then left+w else -1
						| CompoundBranch(l, m) -> let right=(getWeight m) in let rw = l*right in if rw<0 then -1 else if lw=rw then left+right else -1))

let balanced : mobile -> bool
= fun m -> if (getWeight m)>0 then true else false;


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let carry = ZERO

let rec getXElement : bin -> int -> int -> digit
= fun b l x -> if l>=x then ZERO else match b with
					| [] -> ZERO
					| hd::tl -> if l=x+1 then hd else (getXElement tl (l-1) x)

let rec getLength : bin -> int
= fun b -> match b with
		| [] -> 0
		| hd::tl -> (getLength tl) + 1

let rec exten: int -> bin
= fun x -> if x=0 then [] else ZERO::(exten (x-1))

let rec add : bin -> bin -> (digit * bin)
= fun b1 b2 -> match b1, b2 with
		| [], [] -> (ZERO, [])
		| [], _::_ -> (ZERO, [])
		| _::_, [] -> (ZERO, [])
		| hd1::tl1, hd2::tl2 -> let res = (add tl1 tl2) in
						match res with
							|(x,y) -> if hd1=hd2 then
									(hd1, x::y)
								  else if x=ZERO then
										(ZERO, ONE::y)
									else (ONE, ZERO::y)

let addBin : bin -> bin -> int -> bin
= fun b1 b2 x -> let b1 = b1@(exten x) in
			let len1 = (getLength b1) in
			let len2 = (getLength b2) in
			if len1<len2 then
				let b1 = (exten (len2-len1))@b1 in
					let res = (add b1 b2) in
						match res with
							|(x,y) -> if x=ZERO then y else ONE::y
			else let b2 = (exten (len1-len2))@b2 in
					let res = (add b1 b2) in
						match res with
							|(x,y) -> if x=ZERO then y else ONE::y
		

let rec cmul : bin -> digit -> bin
= fun b x -> match b with
		| [] -> []
		| hd::tl -> if hd=ZERO then ZERO::(cmul tl x) else x ::(cmul tl x)

let rec smul : bin -> bin -> int -> bin
= fun b1 b2 x -> match b2 with
			| [] -> []
			| hd::tl -> let first = (cmul b1 hd) in
				    let second = (smul b1 tl (x-1)) in
					addBin first second x

let rec bmul : bin -> bin -> bin
= fun b1 b2 -> (smul b1 b2 ((getLength b2)-1))
