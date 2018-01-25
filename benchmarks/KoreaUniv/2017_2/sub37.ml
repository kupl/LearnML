(* -------------------------------------------- *)
(* -----------------2012210109----------------- *)
(* --------------------김진용-------------------- *)


(* ------------------problem1------------------ *)
type btree = Empty | Node of int * btree * btree
let rec mirror : btree -> btree
= fun t -> 
	match t with
	| Empty -> Empty
	| Node(d,l,r) -> Node(d, mirror r, mirror l);;
(* -------------------------------------------- *)

(* ------------------problem2------------------ *)
type nat = ZERO | SUCC of nat
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

let natadd : nat -> nat -> nat 
= fun n1 n2 -> 
	let rec add nat1 nat2 = 
		match nat2 with
		| ZERO -> nat1
		| SUCC(a) -> add (SUCC(nat1)) a in
	match n2 with
	| ZERO -> n1
	| SUCC(a) -> add n1 n2;;

let natmul : nat -> nat -> nat 
= fun n1 n2 -> 
	let tmp = n1 in
	let rec mul nat1 nat2 = 
		match nat2 with
		| ZERO -> nat1
		| SUCC(a) -> mul (natadd nat1 tmp) a in
	match n2 with
	| ZERO -> ZERO
	| SUCC(a) -> mul n1 a;;


let natexp : nat -> nat -> nat 
= fun n1 n2 -> 
	let tmp = n1 in
	let rec exp nat1 nat2 = 
		match nat2 with
		| ZERO -> nat1
		| SUCC(a) -> exp (natmul nat1 tmp) a in
	match n2 with
	| ZERO -> SUCC ZERO
	| SUCC(a) -> exp n1 a;;
(* -------------------------------------------- *)

(* ------------------problem3------------------ *)
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
	let test fm =
		match fm with
		| Var(s) -> true
		| _ -> false in
	match f with
	| True -> true
	| False -> false
	| Var(s) -> true
	| Neg(f1) -> if not (sat f1) then true else (test f1)
	| And(f1, f2) -> if ((sat f1) && (sat f2)) then true else false
	| Or(f1, f2) -> if((sat f1) || (sat f2)) then true else false
	| Imply(f1, f2) -> if (not (sat f1)) || (test f1) then true else (sat f2)  (*var수정*)
	| Iff(f1, f2) -> (sat (And( Imply(f1, f2), Imply(f2, f1))));;
(* -------------------------------------------- *)

(* ------------------problem4------------------ *)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> 
	let rec sum l = 
		match l with
		| [] -> Const 0
		| hd::tl -> Sum([(diff (hd, x)) ; (sum tl)]) in

	let rec times l = 
		match l with
		| [] -> Const 0
		| hd::tl -> 
			Sum(Times ((diff (hd, x)) ::tl) :: Times ([hd ; (times tl)]) :: []) in

	match e with 
	| Const(n) -> Const 0
	| Var(s) -> if s=x then Const 1 else Const 0
	| Power(s,n) -> if s=x then Times[Const n; Power (s, (n-1))] 
					else Const 0 
	| Times(l) -> times l
	| Sum(l) -> sum l;;
(* -------------------------------------------- *)

(* ------------------problem5------------------ *)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e ->
	let rec div f =
      (match f with
      X -> raise (Failure "Not Assigned")  |
      INT n -> n |
      ADD (e1, e2) -> (div e1) + (div e2) |
      SUB (e1, e2) -> (div e1) - (div e2) |
      MUL (e1, e2) -> (div e1) * (div e2) |
      DIV (e1, e2) -> (div e1) / (div e2) |
      SIGMA (e1, e2, e3) -> 

      let rec calulate k =
         (match k with
         X -> (fun x->x) |
         INT n -> (fun x->n) |
         ADD (e1,e2) -> (fun x->((calulate e1) x)+((calulate e2) x)) |
         SUB (e1,e2) -> (fun x->((calulate e1) x)-((calulate e2) x)) |
         MUL (e1,e2) -> (fun x->((calulate e1) x)*((calulate e2) x)) |
         DIV (e1,e2) -> (fun x->((calulate e1) x)/((calulate e2) x)) |
         SIGMA (e1,e2,e3) -> raise (Failure "test,,,"))
       in

       let rec sigma s f =
         if s>f then 0
         else if s=f then ((calulate e3) s)
         else (((calulate e3) s) + (sigma (s+1) f))
       in (sigma (div e1) (div e2)))
    in

    match e with
    X -> 0 |
    INT n -> n |
    ADD (e1,e2) -> (div (ADD (e1,e2))) |
    SUB (e1,e2) -> (div (SUB (e1,e2))) |
    MUL (e1,e2) -> (div (MUL (e1,e2))) |
    DIV (e1,e2) -> (div (DIV (e1,e2))) |
    SIGMA (e1,e2,e3) -> (div (SIGMA (e1,e2,e3)));;
(* -------------------------------------------- *)

(* ------------------problem6------------------ *)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> (* TODO *)
	let rec getW b =
		match b with
		| SimpleBranch(len,wgt) -> wgt
		| CompoundBranch(len, mob) -> match mob with (l, r) -> (getW l) +(getW r) in
	let rec cal b =
		match b with
		| SimpleBranch(len, wgt) -> len*wgt
		| CompoundBranch(len, mob) -> if not (balanced mob) then (-100)
									else match mob with (l, r) -> len*((getW l)+(getW r)) in
	match m with
	|(left, right) -> if((cal left)= (-100) || (cal right)= (-100)) then false
					  else (cal left)=(cal right);;
					  (*양쪽이 unbalanced일때 가능한지 체크하기 : 판별가능 -> 그대로 두기
														 :안됨 -> 주석 풀고, 다시 체크*)


(* -------------------------------------------- *)

(* ------------------problem7------------------ *)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
	let rec twoPower n = 
		match n with
		| 0 -> 1
		| 1 -> 2
		| _ -> 2 * twoPower (n-1) in
	let rec binToInt b = 
		match b with
		| [] -> 0
		| hd::tl -> if hd=ONE then (twoPower ((List.length b)-1)) + (binToInt tl)
					else binToInt tl in
	let rec intToBin n = 
		match n with
		| 0 -> []
		| _ -> if (n mod 2) = 0 then intToBin (n/2) @ [ZERO]
				else intToBin (n/2) @ [ONE] in

	let tmp1 = binToInt b1 in
	let tmp2 = binToInt b2 in
	intToBin (tmp1*tmp2);;


(* #use "hw2.ml";; *)










