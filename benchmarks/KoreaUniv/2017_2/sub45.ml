(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
	| Empty -> Empty
	| Node(x, left, right) -> Node(x, (mirror right), (mirror left))


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
		| ZERO -> n2
		| SUCC tail -> natadd tail (SUCC n2)

let natmul : nat -> nat -> nat 
= fun n1 n2 -> let rec loop n1 n2 i = 
		match i with
		| ZERO -> ZERO
		| SUCC tail -> natadd n2 (loop n1 n2 tail)
		in loop n1 n2 n1

let natexp : nat -> nat -> nat 
= fun n1 n2 -> let rec loop n1 n2 i = 
		match i with
		| ZERO -> SUCC ZERO
		| SUCC tail -> natmul n1 (loop n1 n2 tail)
		in loop n1 n2 n2


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
	let rec makeBoolTable f table = 
		match f with 
		| True | False -> table
		| Var p -> let rec find_var var var_list =
					match var_list with
					| [] -> ((var, true) :: table)
					| hd :: tl when hd = (var, true) -> table
					| hd :: tl (* when hd <> (var, true) *) -> find_var var tl
				in find_var p table
		| Neg p -> makeBoolTable p table
		| And (p, q) | Or (p, q) | Imply (p, q) | Iff (p, q) 
		-> makeBoolTable q (makeBoolTable p table)
	in

	let rec loopTable t = 
		match t with
		| hd :: tl -> (match hd with
						| (v, true) -> ((v, false) :: tl)
						| (v, false) -> ((v, true) :: (loopTable tl))
						)
		| [] -> []
	in 

	let rec sat_f f table value =
		match f with
		| True when value = true -> true
		| True (* when value = false *) -> false
		| False when value = false -> true 
		| False (* when value = true *) -> false
		| Var p -> let rec find_var var table =
					match table with
					| hd :: tl when hd = (var, value) -> true
					| hd :: tl when hd = (var, not(value)) -> false
					| hd :: tl -> find_var var tl
					| [] -> raise (Failure "RunTimeError: why [] is here?")
				in find_var p table
		| Neg p -> sat_f p table (not(value))
		| And (p, q) -> if value = true then ((sat_f p table true) && (sat_f q table true))
						else (* value = false *)
						let caseFT = ((sat_f p table false) && (sat_f q table true))
						in let caseTF = ((sat_f p table true) && (sat_f q table false))
						in let caseFF = ((sat_f p table false) && (sat_f q table false))
						in (((caseFT) || (caseTF)) || caseFF)

		| Or (p, q) -> sat_f (And ((Neg p), (Neg q))) table (not(value))
		| Imply (p, q) -> sat_f (And (p, (Neg q))) table (not(value))
		| Iff (p, q) -> sat_f (And ((Imply (p, q)), (Imply (q, p)))) table value
	in

	let rec solve_sat f table =
		match table with
		| [] -> false
		| _ -> if (sat_f f table true) then true
					else (sat_f f (loopTable table) true)
	in

	let solve_sat_init f table = 
	(* this init function is for No variable formula like "True and False" *)
		match table with
		| [] -> sat_f f table true
		| _ -> solve_sat f table

	in solve_sat_init f (makeBoolTable f [])



(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
	| Const n -> Const 0
	| Var var when var = x -> Const 1
	| Var var -> Const 0 (* when var <> x *)
	| Power (var, n) when var = x -> 
		if n = 0 then Const 0
		else Times [Power (var, (n-1)); Const n]
	| Power (var, n) (* when var <> x *) -> Const 0
	| Sum list -> (
		match list with
		| [] -> raise (Failure "NullListError: list argument is null")
		| hd :: tl when tl <> [] -> Sum (diff (hd, x) :: [(diff ((Sum tl), x))])
		| hd :: tl (* when tl = [] *) -> (diff (hd, x))
	)
	| Times list -> (
		match list with
		| [] -> raise (Failure "NullListError: list argument is null")
		| hd :: tl when tl <> [] -> Sum ((Times ((diff (hd, x)) :: tl)) :: [Times (hd :: [(diff ((Times tl), x))])])
		| hd :: tl (* when tl = [] *) -> (diff (hd, x))
	)


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp;;

let calculator : exp -> int
= fun e -> let rec loop e xAssigned xValue =
	match e with
	| X -> if xAssigned = false
			then raise (Failure "XnotAssignedError: var X is only able to be used in SIGMA")
		else xValue

	| INT n -> n

	| ADD(e1, e2) -> 
		((loop e1 xAssigned xValue) + (loop e2 xAssigned xValue))

	| SUB(e1, e2) -> 
		((loop e1 xAssigned xValue) - (loop e2 xAssigned xValue))

	| MUL(e1, e2) ->
		((loop e1 xAssigned xValue) * (loop e2 xAssigned xValue))

	| DIV(e1, e2) -> 
		let n2 = loop e2 xAssigned xValue in 
		if n2 = 0 then raise (Failure "DivideByZeroError: impossible to divide an integer by zero")
		else ((loop e1 xAssigned xValue) / n2)

	| SIGMA(e1, e2, f) -> 	
		let n1 = loop e1 xAssigned xValue in
		let n2 = loop e2 xAssigned xValue in
		let sum = 0 in
		if n1 > n2 then raise (Failure "SIGMAformError: n1 should be less or equal then n2")
		else let rec cal_sigma n = 
			if n > n2 then sum
			else (sum + (loop f true n) + (cal_sigma (n+1))) 
		in cal_sigma n1

	in (loop e false 0)
	


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int;;

let balanced : mobile -> bool
= fun m -> let rec cal_weight b =
	match b with
	| SimpleBranch(l, w) when ((l > 0) && (w > 0)) -> w
	| CompoundBranch(l, (leftb, rightb)) when l > 0 -> ((cal_weight leftb) + (cal_weight rightb)) 
	| _ -> raise (Failure "NotPositiveError: length and weight should be positive") in

	let rec cal_balanced m = match m with 
	| (SimpleBranch(leftx, leftw), SimpleBranch(rightx, rightw)) -> ((leftx * leftw) = (rightx * rightw))
	| (((CompoundBranch(leftx, leftm)) as left), right) -> 
		if (cal_balanced leftm) = true 
			then cal_balanced (SimpleBranch(leftx, (cal_weight left)), right)
		else false
	| (left, ((CompoundBranch(rightx, rightm)) as right)) -> 
		if (cal_balanced rightm) = true 
			then cal_balanced (left, SimpleBranch(rightx, (cal_weight right)))
		else false	 
	
	in cal_balanced m;;



(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> 
	let rec reverse list =
	match list with
	| [] -> []
	| hd :: tl -> (reverse tl) @ [hd] in

	let rec binadd b1 b2 carry =
	match b1, b2 with
	| [], [] when carry = ZERO -> []
	| [], [] (* when carry = ONE *) -> [ONE]
	| [], (hd :: tl) -> binadd [ZERO] b2 carry
	| (hd :: tl), [] -> binadd b1 [ZERO] carry
	| (hd1 :: tl1), (hd2 :: tl2) ->
		match hd1, hd2, carry with
		| ZERO, ZERO, ZERO 
		-> ZERO :: (binadd tl1 tl2 ZERO)
		| ZERO, ZERO, ONE | ZERO, ONE, ZERO | ONE, ZERO, ZERO
		-> ONE :: (binadd tl1 tl2 ZERO)
		| ZERO, ONE, ONE | ONE, ZERO, ONE | ONE, ONE, ZERO 
		-> ZERO :: (binadd tl1 tl2 ONE)
		| ONE, ONE, ONE
		-> ONE :: (binadd tl1 tl2 ONE) in

	let rec binmul b1 b2 =
		match b2 with
		| [] -> [ZERO]
		| hd :: tl when hd = ZERO -> binmul (ZERO :: b1) tl
		| hd :: tl (* when hd = ONE *) -> binadd b1 (binmul (ZERO :: b1) tl) ZERO
	
	in match b1, b2 with
	| [], _ | _, [] -> raise (Failure "NullListError: one or both list are null")
	| _, _ -> reverse (binmul (reverse b1) (reverse b2))