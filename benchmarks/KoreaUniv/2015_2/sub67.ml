(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
match lst with
[] -> []
|h::t -> if pred h = true then h::(filter pred t) else (filter pred t);;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
[] -> b
|h::t -> h::(zipper (b, t));;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> if n < 0 then raise (Failure "n should not be negative") else
if n = 0 then fun x -> x else fun x -> f (iter(n-1, f) x);;

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
=fun (aexp,x) -> match aexp with
Const i -> Const 0
|Var k -> if k = x then Const 1 else Var k
|Power (k, i) -> if k = x then Times[Const i; Power(k, i-1)] else
	Power(k, i)
|Times l -> (match l with
	[] -> Const 0
	|h::t -> (match h with
		Var k -> if k = x then Times ((diff (h, x))::t) else
			Times[h; diff(Times t, x)]
		|Power (k, i) -> if k = x then Times ((diff (h, x))::t) else
			Times[h; diff(Times t, x)]
		|_ -> Times [h; diff(Times t, x)]))
|Sum l -> (match l with
	[] -> Const 0
	|h::t -> Sum [diff(h, x); diff(Sum t, x)]);;



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
let rec calculator : exp -> int
=fun e -> match e with
X -> raise (Failure "undefined X")
|INT i -> i
|ADD (e1, e2) -> (calculator e1) + (calculator e2)
|SUB (e1, e2) -> (calculator e1) - (calculator e2)
|MUL (e1, e2) -> (calculator e1) * (calculator e2)
|DIV (e1, e2) -> (calculator e1) / (calculator e2)
|SIGMA (e1, e2, e3) -> if e1 <= e2 then
	let rec cal g3 g1 = match g3 with
		X -> calculator g1
		|INT i -> i
		|ADD (e1, e2) -> (cal e1 g1) + (cal e2 g1)
		|SUB (e1, e2) -> (cal e1 g1) - (cal e2 g1)
		|MUL (e1, e2) -> (cal e1 g1) * (cal e2 g1)
		|DIV (e1, e2) -> (cal e1 g1) / (cal e2 g1)
		|SIGMA (e1, e2, e3) -> calculator (SIGMA (e1, e2, e3)) in
	(cal e3 e1) + calculator (SIGMA (INT ((calculator e1) + 1), e2, e3)) else
	0;;


