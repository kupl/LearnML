(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
	match lst with
	| [] -> []
	| h::t -> if (pred h) then h::filter pred t else filter pred t;;


(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
	match a with
	| [] -> (match b with
		| [] -> [];
		| hb::tb -> hb::zipper (a, tb))
	| ha::ta -> (match b with
		| [] -> ha::zipper (ta, b)
		| hb::tb -> ha::hb::zipper(ta, tb));;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec help3 n f r =
	if (n = 0) then
		r
	else
		f (help3 (n-1) f r)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> (fun a -> help3 n f a);;

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> 
	match aexp with
	| Const c -> Const 0
	| Var a -> if (a = x) then Const 1 else Const 0
	| Power (a, n) -> Times [Const n; Power(a, n-1)]
	| Times l -> Sum (help_times(l, l, 0, x))
	| Sum l -> Sum (help_sum (l, x))
and help_sum (l, x) =
	match l with
	| [] -> []
	| h::t -> (diff (h,x))::help_sum (t, x)
and help_times1 (l, n, x) =
	match l with
	| [] -> []
	| h::t -> if (n = 0) then help_times1 (t, n-1, x) else h::help_times1 (t, n-1, x)
and help_times (ll, l, n, x) =
	match l with
	| [] -> []
	| h::t -> (Times (diff(h, x)::help_times1(ll, n, x)))::help_times(ll, t, n+1, x)

(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calc_x ex ei =
	match ex with
	| X -> ei
	| INT i -> i
	| ADD (a, b) -> calc_x a ei + calc_x b ei
	| SUB (a, b) -> calc_x a ei - calc_x b ei
	| MUL (a, b) -> calc_x a ei * calc_x b ei
	| DIV (a, b) -> calc_x a ei / calc_x b ei
	| _ -> 0

let rec calc_sum ei ej ex =
	if (ei = ej) then 
		calc_x ex ei
	else
		(calc_x ex ei + calc_sum (ei+1) ej ex)

let calculator : exp -> int
=fun e -> 
	match e with
	| SIGMA (ei, ej, ex) -> calc_sum (calc_x ei 0) (calc_x ej 0) ex
	| _ -> 0;;