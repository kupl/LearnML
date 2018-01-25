(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec  mirror : btree->btree
=fun t -> match t with
	|Empty -> Empty
	|Node (a, Empty, Empty) -> t
	|Node (a, Empty, right) -> Node(a, mirror (right), Empty)
	|Node (a, left, Empty) -> Node(a, Empty, mirror(left)) 
	|Node (a, left, right) -> Node (a, mirror (right), mirror (left));;

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
|ZERO -> n2
|SUCC a -> SUCC(natadd a n2);;

let rec natmul : nat -> nat ->nat
= fun n1 n2 -> match n1 with
|ZERO -> ZERO
|SUCC a -> natadd n2 (natmul a n2);;

let rec natexp : nat -> nat -> nat
= fun n1 n2 -> match n2 with
|ZERO -> SUCC ZERO
|SUCC a -> natmul n1 (natexp n1 a);;

(* problem 3*)
type formula =
 |True
 |False
 |Var of string
 |Neg of formula
 |And of formula * formula
 |Or of formula * formula
 |Imply of formula * formula
 |Iff of formula * formula

let rec sat : formula -> bool
=fun f -> match f with
|True -> true
|False -> false
|Var v -> if v = "P" then true else false
|Neg n -> if (sat n) == true then false else true
|And (a, b) -> if ((sat a) == true) && ((sat b) == true) then true else false
|Or (a, b) -> if((sat a) == true) || ((sat b) == true) then true else false
|Imply (a, b) -> if (sat a) = true then true 
				else if (sat b) == true then true else false
|Iff (a, b) -> if ((sat a)==true) && ((sat b)==true) then true
				else if ((sat b)==false) && ((sat b)==false) then true
				else false;;

(* problem 4*)
type aexp = 
	|Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> begin
	match e with
	|Const a -> Const 0
	|Var v -> if v = x then Const 1 else Const 0
	|Power (v, i) -> if i > 0 then Times[Const i; Power(v, i-1)]
					else Const 0
	|Times lst -> (begin
		match lst with
		|[] -> Const 0
		|hd::[] -> diff(hd, x)
		|hd::tl -> Sum[Times(diff(hd, x)::tl); Times[hd; diff(Times tl, x)]]
	end)
	|Sum lst -> begin
		match lst with
		|[] -> Const 0
		|hd::[] -> diff(hd, x)
		|hd::tl -> Sum[diff(hd, x); diff(Sum tl, x)]
	end
end;;

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
let rec calc : exp -> int -> int
= fun ex i -> (match ex with
  | X -> i
  | INT k -> k
  | ADD (a,b) -> (calc a i) + (calc b i)
  | SUB (a,b) -> (calc a i) - (calc b i)
  | MUL (a,b) -> (calc a i) * (calc b i)
  | DIV (a,b) -> if (calc b i) = 0 then raise (Failure "divide by 0 error")
                 else (calc a i) / (calc b i)
  | SIGMA (s, e, f) -> let sta = calc s i in 
                          let ed = calc e i in
                          if (sta = ed) then calc f sta
                          else (calc f sta) + (calc (SIGMA ((INT (sta+1)),INT ed,f))i))
in match e with
  | X -> raise(Failure "Error")
  | INT i -> i
  | ADD (a,b) -> calculator(a) + calculator(b)
  | SUB (a,b) -> calculator(a) - calculator(b)
  | MUL (a,b) -> calculator(a) * calculator(b)
  | DIV (a,b) -> if calculator(b) = 0 then raise (Failure "not divisiable by 0")
                 else calculator(a) / calculator(b)
  | SIGMA (sta, en, f) -> let st = calculator(sta) in
                              let ed = calculator(en) in
                                if (st = ed) then calc f st
																else (calc f st) + (calculator( SIGMA( INT (st + 1), INT ed, f)));;
(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> 
let rec weight : branch -> weight
= fun w -> match w with 
	| SimpleBranch (l, w) -> w
	| CompoundBranch (l, m) ->( begin
		match m with 
		| (left,right) -> (weight left) + (weight right)
	end)
in let calc_Torq : branch -> int
=fun torq -> begin
	match torq with
	| SimpleBranch (l, w) -> l*w
	| CompoundBranch (l, m) -> (match m with
		|(left, right) -> l*(weight left) + l*(weight right))
end
in match m with
| (left, right) -> (match left, right with
										| SimpleBranch (l1, w1), SimpleBranch (l2, w2) -> if (calc_Torq left) = (calc_Torq right) then true else false
										| SimpleBranch (l1, w2), CompoundBranch (l2, m2) -> if (balanced m2 = true) && (calc_Torq left = calc_Torq right) then true else false
										| CompoundBranch (l1, m1), SimpleBranch (l2, w2) -> if (balanced m1 = true) && (calc_Torq left = calc_Torq left) then true else false
										| CompoundBranch (l1, m1), CompoundBranch (l2, m2) -> 
																								if (balanced m1 = true)  && (balanced m2 = true) && (calc_Torq left = calc_Torq right) then true else false);;


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bmul : bin -> bin -> bin
= fun b1 b2 -> 
let rec pow : int -> int
= fun p  -> if p = 0 then 1 else 2*pow(p - 1)
in let b_digit : digit -> int
= fun bdigit -> 
begin
	match bdigit with
	| ZERO -> 0
	| ONE -> 1
end
in let rec conv_bin : bin -> int
= fun lst -> (
	match lst with
	| [] -> raise (Failure "Empty Binary List")
	| hd::[] -> b_digit(hd)
	| hd::tl -> b_digit(hd)*(pow ((List.length lst)-1)) + conv_bin(tl))
in let calc_mul : int -> int -> int
= fun a b -> a*b
in let rec conv_int : int -> bin
= fun x -> if x = 0 then [ZERO]
					else if x = 1 then [ONE]
					else List.append (conv_int(x/2)) (conv_int(x mod 2))
in conv_int((calc_mul (conv_bin(b1))  (conv_bin(b2))));;

