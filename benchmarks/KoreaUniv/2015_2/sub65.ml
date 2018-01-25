(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = match lst with
	| [] -> []
	| hd :: tl -> if (pred hd) then (hd)::(filter pred tl)  else  (filter pred tl)


(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
	if (a=[] && b=[]) then []
	else if (a=[]) then
		match b with
		| [] -> []
		| hd :: tl -> (hd)::(zipper (a, tl))
	else if (b=[]) then
		match a with
		| [] -> []
		| hd :: tl -> (hd)::(zipper (tl, b))
	else
		match a with
		| [] -> []
		| hd :: tl ->
			match b with
			| [] -> []
			| x :: y -> (hd)::(x)::(zipper (tl, y))
(*******************)
(* Problem 3: iter *)
(*******************)
exception Invalid

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->
	if n=0 then fun x->x
	else if n>0 then let k = iter(n-1,f) in fun x-> k (f x)
	else raise Invalid

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

exception Inval

let rec diff : aexp * string -> aexp
= fun (aexp,x) ->
	match aexp with
	| Const n -> Const 0
	| Var k -> if k=x then Const 1 else Const 0
	| Power (k, n) ->
		if k=x then 
			if n=0 then Const 0
			else if n=1 then Const 1
			else Times [Const n ; Power(x, n-1)]
		else Const 0
	| Times l ->
		(
		if (List.mem (Const 0) l) then Const 0
		else 
			match l with
			| [] -> raise Inval
			| hd :: [] -> (diff (hd, x))
			| hd :: tl ->
				match hd with
				| Const 1 -> (diff ((Times tl), x))
				| Const n -> Times [hd; (diff ((Times tl), x))]
				| _ -> Sum ([(Times ((diff (hd, x)):: tl); (Times [hd;(diff ((Times tl), x))]))])
		)
	| Sum l ->
		match l with
		| [] -> raise Inval
		| hd :: [] -> (diff (hd, x))
		| hd :: tl -> Sum [(diff (hd, x)); (diff ((Sum tl),x))]

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

exception Fault

let rec eval (exp, xval) =
	match (exp, xval) with
	 | (X, INT x) -> x
	 | (X, _) -> raise Fault 
	 | (INT i, _) -> i
	 | (ADD (exp1, exp2), _) ->
	 	(
		match exp1, exp2 with
		| INT x, INT y -> x+y
		| _, _ -> (eval (exp1, xval) + eval (exp2, xval))
		)
	| (SUB (exp1, exp2), _) ->
		(           
		match exp1, exp2 with                   
		| INT x, INT y -> x-y
		| _, _ -> (eval (exp1, xval) - eval (exp2, xval))
		)
	| (MUL (exp1, exp2), _) ->
		(
		match exp1, exp2 with                       
		| INT x, INT y -> x*y
		| _, _ -> (eval (exp1, xval) * eval (exp2, xval))
		)                                           
	| (DIV (exp1, exp2), _) ->                                                      
		(                                                                       
		match exp1, exp2 with                                                                                       
		| INT x, INT y -> x/y  
		| _, _ -> (eval (exp1, xval) / eval (exp2, xval))      
		)                                     
	| (SIGMA (exp1, exp2, exp3), _) ->
		let x = eval (exp1, xval)
		and y = eval (exp2, xval) in
		(
		if x>y then 0
		else if x=y then eval (exp3, INT x)
		else eval (exp3, INT x) + eval (SIGMA(INT (x+1), INT y, exp3), xval)
		)

let calculator : exp -> int
=fun e -> eval (e, X)
