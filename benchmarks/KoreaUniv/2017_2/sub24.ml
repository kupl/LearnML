(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> 
  match t with
  | Empty -> t
  | Node (i, b1, b2) -> Node (i, mirror b2, mirror b1)
;;

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC (nat) -> SUCC (natadd nat n2)
;;

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC (nat) -> natadd (natmul nat n2) n2
;;

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  | ZERO -> SUCC ZERO (* A lot of people argue that 0^0=1 *)
  | SUCC (nat2) -> (
    match n1 with
    | ZERO -> ZERO
    | SUCC (nat1) -> natmul n1 (natexp n1 nat2)
  )
;;

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

type new_bool =
  | T
  | F
  | Undef of string
  | NegUndef of string

let rec sat_rec : formula -> new_bool
= fun f ->
	match f with
	| True -> T
	| False -> F
	| Var v -> Undef v
	| Neg f -> (
		let nb = sat_rec f in (* nb: new bool *)
		match nb with
		| T -> F
		| F -> T
		| Undef v -> NegUndef v
		| NegUndef v -> Undef v
	)
	| And (f1, f2) -> (
		let (nb1, nb2) = ((sat_rec f1), (sat_rec f2)) in
		if nb1=F || nb2=F then F
		else if nb1=T && nb2=T then T
		else if nb1=T then nb2
		else if nb2=T then nb1
		else ( (* nb1 and nb2 are both not one of T or F*)
			match nb1 with
			| Undef v1 -> (
				match nb2 with
				| Undef v2 -> T
				| NegUndef v2 -> if v1=v2 then F else T
				| _ -> T
			)
			| NegUndef v1 -> (
				match nb2 with
				| Undef v2 -> if v1=v2 then F else T
				| NegUndef v2 -> T
				| _ -> T
			)
			| _ -> T
		) 
	)
	| Or (f1, f2) -> (
		let (nb1, nb2) = ((sat_rec f1), (sat_rec f2)) in
		if nb1=F && nb2=F then F else T
	)
	| Imply (f1, f2) -> (
		let (nb1, nb2) = ((sat_rec f1), (sat_rec f2)) in
		match nb1 with
		| T -> nb2
		| F -> T
		| Undef v1 -> (
			match nb2 with
			| NegUndef v2 -> if v1=v2 then Undef v1 else T
			| _ -> T
		)
		| NegUndef v1 -> (
			match nb2 with
			| Undef v2 -> if v1=v2 then NegUndef v1 else T
			| _ -> T
		)
	)
	| Iff (f1, f2) -> (
		let (nb1, nb2) = ((sat_rec f1), (sat_rec f2)) in
		match nb1 with
		| T -> nb2
		| F -> (
			match nb2 with
			| T -> F
			| F -> T
			| Undef v2 -> NegUndef v2
			| NegUndef v2 -> Undef v2
		)
		| Undef v1 -> (
			match nb2 with
			| T -> nb1
			| F -> NegUndef v1
			| Undef v2 -> T
			| NegUndef v2 -> if v1=v2 then F else T
		)
		| NegUndef v1 -> (
			match nb2 with
			| T -> Undef v1
			| F -> nb1
			| Undef v2 -> if v1=v2 then F else T
			| NegUndef v2 -> T
		)
	)
;;

let sat : formula -> bool
= fun f -> 
	let sat_result = sat_rec f in
	if sat_result=F then false else true
;;

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec change_elem : aexp list -> int -> aexp -> aexp list
= fun l i a ->
	match l with
	| hd::tl -> if i = 0 then a::tl else hd::(change_elem tl (i-1) a)
	| [] -> []
;;

let rec diff : aexp * string -> aexp
= fun (e,x) -> 
	match e with
	| Const i -> Const 0
	| Var v -> if x = v then Const 1 else Const 0
	| Power (v, i) -> (
		if x = v then (
			if i = 0 then Const 0
			else if i = 1 then Const 1
			else (
				let diff_pow = if i = 2 then Var v else (Power (v, (i-1))) in
				Times [(Const i); diff_pow] 
			)
		)
		else Const 0
	)
	| Times l -> (
		let result_list = ref [] in
		for i = 0 to (List.length l) - 1 do
			let temp_list = l in
			let ith_diff = diff ((List.nth l i), x) in
			let temp_list = (change_elem temp_list i ith_diff) in
			result_list := !result_list @ [Times temp_list]
		done;
		Sum !result_list
	)
	| Sum l -> (
		let result_list = ref [] in
		for i = 0 to (List.length l) - 1 do
			result_list := !result_list @ [diff ((List.nth l i), x)]
		done;
		Sum !result_list
	)
;;

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let temp_x = ref (-12541);;
let rec calculator : exp -> int
= fun e ->
	match e with
	| X -> if !temp_x = (-12541) then raise (Failure "Non-initialized variable error") else !temp_x
	| INT i -> i
	| ADD (e1, e2) -> (calculator e1) + (calculator e2)
	| SUB (e1, e2) -> (calculator e1) - (calculator e2)
	| MUL (e1, e2) -> (calculator e1) * (calculator e2)
	| DIV (e1, e2) -> (
		let cal_e2 = calculator e2 in
		match cal_e2 with
		| 0 -> raise (Failure "divide by zero error")
		| _ -> (calculator e1) / cal_e2
	)
	| SIGMA (e1, e2, e3) -> (		
		let old_temp_x = !temp_x in
		let cal_e1 = calculator e1 in
		let cal_e2 = calculator e2 in
		temp_x := cal_e1;
		let cal_e3 = calculator e3 in
		temp_x := old_temp_x;
		if cal_e1 > cal_e2 then (raise (Failure "e2 larger than e1 error"))
		else if cal_e1 == cal_e2 then cal_e3
		else cal_e3 + (calculator (SIGMA (INT (cal_e1 + 1), e2, e3)))
	)
;;

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec compound_to_simple : (length * mobile) -> (length * weight)
= fun (l, m) ->
	match m with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> (l, w1 + w2)
	| (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> (
		let (_, w2) = compound_to_simple (l2, m2) in
		(l, w1 + w2)
	)
	| (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> (
		let (_, w1) = compound_to_simple (l1, m1) in
		(l, w1 + w2)
	)
	| (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> (
		let (_, w1) = compound_to_simple (l1, m1) in
		let (_, w2) = compound_to_simple (l2, m2) in
		(l, w1 + w2)
	)
;;

let rec balanced : mobile -> bool
= fun m ->
	match m with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> (
		if (l1 * w1) == (l2 * w2) then true
		else false
	)
	| (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> (
		if (balanced m2) then (
			let (_, w2) = compound_to_simple (l2, m2) in
			if (l1 * w1) == (l2 * w2) then true
			else false
		)
		else false
	)
	| (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> (
		if (balanced m1) then (
			let (_, w1) = compound_to_simple (l1, m1) in
			if (l1 * w1) == (l2 * w2) then true
			else false
		)
		else false
	)
	| (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> (
		if (balanced m1) && (balanced m2) then (
			let (_, w1) = compound_to_simple (l1, m1) in
			let (_, w2) = compound_to_simple (l2, m2) in
			if (l1 * w1) == (l2 * w2) then true
			else false
		)
		else false
	)
;;

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec dig_mul : bin -> digit -> bin
= fun b d ->
	if d = ZERO then [ZERO]
	else (
		match b with
		| hd::tl -> (
			if hd = ZERO then ZERO::(dig_mul tl d)
			else ONE::(dig_mul tl d)
		)
		| [] -> []
	)
;;

let rec dig_mul_all : bin -> bin -> int -> bin list
= fun b1 b2 zero_tl->
	match b2 with
	| hd::tl -> (
		let mul_result = ref (dig_mul b1 hd) in
		if !mul_result != [] then (
			for i = 1 to zero_tl do
				mul_result := (!mul_result)@[ZERO]
			done;
			(!mul_result)::(dig_mul_all b1 tl (zero_tl - 1))
		)
		else (dig_mul_all b1 tl (zero_tl - 1))
	)
	| [] -> []
;;

let dig_sum : (digit * digit * digit) -> (digit * digit)
= fun (d1, d2, remain) ->
	let count = ref 0 in
	if d1 = ONE then count := !count + 1;
	if d2 = ONE then count := !count + 1;
	if remain = ONE then count := !count + 1;
	if !count = 0 then (ZERO, ZERO)
	else if !count = 1 then (ONE, ZERO)
	else if !count = 2 then (ZERO, ONE)
	else (ONE, ONE)
;;

let rec dig_sum_list : bin -> bin -> digit -> bin
= fun b1 b2 remain ->
	match b1 with
	| hd1::tl1 -> (
		match b2 with
		| hd2::tl2 -> (
			let (result, new_remain) = dig_sum (hd1, hd2, remain) in
			result::(dig_sum_list tl1 tl2 new_remain)
		)
		| [] -> (
			let (result, new_remain) = dig_sum (hd1, ZERO, remain) in
			result::(dig_sum_list tl1 b2 new_remain)
		)
	)
	| [] -> (
		match b2 with
		| hd2::tl2 -> (
			let (result, new_remain) = dig_sum (ZERO, hd2, remain) in
			result::(dig_sum_list b1 tl2 new_remain)
		)
		| [] -> (
			if remain = ONE then [ONE]
			else []
		)
	)
;;

let rec dig_sum_list_rec : bin -> bin list -> bin
= fun b l ->
	match l with
	| hd::tl -> (
		let sum = dig_sum_list b hd ZERO in
		dig_sum_list_rec sum tl
	)
	| [] -> b
;;

let rec rev_list : bin list -> bin list
= fun l ->
	match l with
	| hd::tl -> (List.rev hd)::(rev_list tl)
	| [] -> []
;;

let rec erase_zero_head : bin -> bin
= fun b ->
	match b with
	| [ZERO] -> b
	| hd::tl -> if hd = ZERO then erase_zero_head tl else b
	| [] -> []
;;

let bmul : bin -> bin -> bin
= fun b1 b2 ->
	if b1 = [] || b2 = [] then raise (Failure "Empty list error")
	else
	(
		let b_list = dig_mul_all b1 b2 ((List.length b2)-1) in
		let rev_b_list = rev_list b_list in
		match rev_b_list with
		| hd::tl -> erase_zero_head (List.rev (dig_sum_list_rec hd tl))
		| [] -> raise (Failure "Empty list error")
	)
;;