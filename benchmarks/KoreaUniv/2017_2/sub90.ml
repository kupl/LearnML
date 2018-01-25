(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
		| Node (x, left, right) -> Node (x, mirror right, mirror left)
		| Empty -> Empty


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
		| SUCC n -> SUCC (natadd n n2)
		| ZERO -> n2

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
		| SUCC n -> natadd n2 (natmul n n2)
		| ZERO -> ZERO

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
		| SUCC n -> natmul n1 (natexp n1 n)
		| ZERO -> (SUCC ZERO)


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

let addVar : string -> string list -> string list 
= fun var var_list -> var_list@[var]

let rec getVarNum : string -> string list -> int
= fun var var_list ->
	match var_list with
		| hd::tail -> if (compare hd var) <> 0 then (1 + ( getVarNum var tail ))
			      else 1
		| [] -> 0

let rec getVal n val_list = 
	if n = 1 then match val_list with hd::tail -> hd | [] -> 0
	else match val_list with
		| hd::tail -> getVal (n-1) tail
		| [] -> 0

let cal_AND p q = 
	if (p = 1) && (q = 1) then 1
	else 0

let cal_OR p q = 
	if (p = 1) || (q = 1) then 1
	else 0

let cal_NEG p = 
	if p = 1 then 0
	else 1

let cal_Imply p q = if (p = 1) && (q = 1) then 1
		    else if (p = 1) && (q = 0) then 0
		    else if (p = 0) && (q = 1) then 1
		    else 1

let cal_Iff : int -> int -> int
= fun p q -> if p = q then 1
		  else 0

let rec makeVarList form var_list =
	match form with
		| Var var -> if (getVarNum var var_list) = 0 then [var]
			     else []
		| And (f1, f2) -> (makeVarList f1 var_list)@(makeVarList f2 var_list)
		| Or (f1, f2) -> (makeVarList f1 var_list)@(makeVarList f2 var_list)
		| Iff (f1, f2) -> (makeVarList f1 var_list)@(makeVarList f2 var_list)
		| Imply (f1, f2) -> (makeVarList f1 var_list)@(makeVarList f2 var_list)
		| Neg (f) -> makeVarList f var_list
		| _ -> []

let rec calculate target var_list val_list = 
	match target with
		| Var var -> getVal (getVarNum var var_list) val_list
		| And (f1, f2) -> cal_AND (calculate f1 var_list val_list) (calculate f2 var_list val_list)
		| Or (f1, f2) -> cal_OR (calculate f1 var_list val_list) (calculate f2 var_list val_list)
		| Iff (f1, f2) -> cal_Iff (calculate f1 var_list val_list) (calculate f2 var_list val_list)
		| Imply (f1, f2) -> cal_Imply (calculate f1 var_list val_list) (calculate f2 var_list val_list)
		| Neg (f) -> cal_NEG (calculate f var_list val_list)
		| True -> 1
		| False -> 0

let rec calAllCases formul var_list val_list var_left = 
	match var_left with
		| [] -> calculate formul var_list val_list
		| hd::tail -> (calAllCases formul var_list (val_list@[1]) tail) + (calAllCases formul var_list (val_list@[0]) tail)

let sat : formula -> bool
= fun f -> 
	let var_list = makeVarList f []
	in if (calAllCases f var_list [] var_list) = 0 then false
	   else true



(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list	

let rec diff : aexp * string -> aexp
 = fun (e, x) -> 
	match e with
		| Const n -> (Const 0)
    		| Times t -> (match t with
              			| hd::tl -> (Sum ([Times ([diff (hd, x)] @ tl)] @ [ Times ([hd] @ [diff (Times tl, x)])]))
				| [] -> (Const 0))
    		| Sum s -> (match s with
            			| hd::tl -> (Sum ([diff (hd, x)] @ [diff (Sum tl, x)]))
				| [] -> (Const 0))
		| Var var -> if (compare x var) = 0 then (Const 1) else (Const 0)
		| Power (var, exp) -> if (compare x var) = 0 then (Times ([Const exp] @ [Power (var,exp-1)])) else (Const 0)


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let rec cal_sigma n m expression
= match expression with
	| ADD (a, b) -> (cal_sigma n m a ) + (cal_sigma n m b)
	| SUB (a, b) -> (cal_sigma n m a) - (cal_sigma n m b)
	| DIV (a, b) -> (cal_sigma n m a) / (cal_sigma n m b)
	| MUL (a, b) -> (cal_sigma n m a) * (cal_sigma n m b)
	| INT k -> k
	| X -> n
	| SIGMA (INT a, INT b, c) -> if a <= b then (cal_sigma a b c) + (cal_sigma (a+1) b (SIGMA (INT (a+1), (INT b), c)))
			     else 0
	| _ -> 0

let rec calculator : exp -> int
= fun e -> 
	match e with
		| SIGMA (INT a, INT b, c) -> if a <= b then (cal_sigma a b c) + calculator (SIGMA ((INT (a+1)), (INT b), c))
				     else 0
		| ADD (a, b) -> (calculator a) + (calculator b)
		| SUB (a, b) -> (calculator a) - (calculator b)
		| DIV (a, b) -> (calculator a) / (calculator b)
		| MUL (a, b) -> (calculator a) * (calculator b)
		| INT n -> n
		| X -> 0
		| _ -> 0


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec get_weight br
= match br with
	| SimpleBranch (l, w) -> w
	| CompoundBranch (l, m) -> (match m with
					| (left, right) -> ((get_weight left) + (get_weight right)))
let get_torque br
= match br with
	| SimpleBranch (l, w) -> l*(get_weight br)
	| CompoundBranch (l, m) -> l*(get_weight br)

let rec balanced : mobile -> bool
= fun m -> match m with
		| (left, right) -> (match left with
					| CompoundBranch (l, m) -> (match right with
									| CompoundBranch (l2,m2) -> if ((get_torque left) = (get_torque right)) && ((balanced m) = true) && ((balanced m2) = true) then true else false
									| SimpleBranch (l2, w2) -> if ((get_torque left) = (get_torque right)) && ((balanced m) = true) then true else false)
					| SimpleBranch (l, w) -> (match right with
									| CompoundBranch (l2, m2) -> if ((get_torque left) = (get_torque right)) && ((balanced m2) = true) then true else false
									| SimpleBranch (l2, w2) -> if (get_torque left) = (get_torque right) then true else false ))


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec shift n b 
= if n < 0 then [] else (shift (n-1) [ZERO]@b)
	
let rec reverse b =
	match b with
		| hd::tl -> (reverse tl)@[hd]
		| [] -> []

let rec reversed_add b1 b2 carry
= match b1 with
	| hd::tl -> (match hd with
			| ZERO -> (match b2 with
					| h::t -> (match h with
							| ZERO -> if carry = 0 then [ZERO]@(reversed_add tl t 0) else [ONE]@(reversed_add tl t 0)
							| ONE -> if carry = 0 then [ONE]@(reversed_add tl t 0) else [ZERO]@(reversed_add tl t 1))
					| [] -> if carry = 0 then [ZERO]@(reversed_add tl [] 0) else [ONE]@(reversed_add tl [] 0))
			| ONE -> (match b2 with
					| h::t -> (match h with
							| ZERO -> if carry = 0 then [ONE]@(reversed_add tl t 0) else [ZERO]@(reversed_add tl t 1)
							| ONE -> if carry = 0 then [ZERO]@(reversed_add tl t 1) else [ONE]@(reversed_add tl t 1))
					| [] -> if carry = 0 then [ONE]@(reversed_add tl [] 0) else [ZERO]@(reversed_add tl [] 1)))
	| [] -> (match b2 with 
		| hd::tl -> (match hd with
			| ZERO -> if carry = 0 then [ZERO]@(reversed_add [] tl 0) else [ONE]@(reversed_add [] tl 0)
			| ONE -> if carry = 0 then [ONE]@(reversed_add [] tl 0) else [ZERO]@(reversed_add [] tl 1))
		| [] -> if carry = 1 then [ONE] else [])


let rec reversed_multiply b1 b2 n
= match b1 with
	| hd::tl -> if hd = ONE then (reversed_add (shift n b2) (reversed_multiply tl b2 (n+1)) 0) else (reversed_add [ZERO] (reversed_multiply tl b2 (n+1)) 0)
	| [] -> [ZERO]

let rec remove_msb_zero b =
	match b with
		| hd::tl -> if hd = ZERO then remove_msb_zero tl else b
		| [] -> [ZERO]

let bmul : bin -> bin -> bin
= fun b1 b2 -> match b1 with
			| hd::tl -> (match b2 with
					| hd::tl -> remove_msb_zero (reverse(reversed_multiply (reverse b1) (reverse b2) 0))
					| [] -> remove_msb_zero (reverse(reversed_multiply (reverse b1) [ZERO] 0)))
			| [] -> (match b2 with
					| hd::tl -> remove_msb_zero (reverse(reversed_multiply [ZERO] (reverse b2) 0))
					| [] -> remove_msb_zero (reverse(reversed_multiply [ZERO] [ZERO] 0)))

