(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
match lst with
|[] -> []
|hd::tl -> if (pred hd) then (hd::(filter pred tl))
else (filter pred tl);;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
match a with
|[] -> b
|hd::tl -> 
	(match b with
	|[] -> a
	|hd2::tl2->[hd;hd2]@zipper(tl,tl2));;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->
match n with
|0 -> fun x -> x
|1 -> f
|_ -> fun x -> f(iter(n-1,f) x);;


(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list
  |Times l ->
	(match l with
		|[] -> Const 0
		|hd::tl -> Sum[Times((diff (hd,x))::tl);Times((hd)::diff(Times tl,x)::[])]
	)
  |Sum l ->
	(match l with
		|[] -> Const 0
		|hd::tl -> Sum[diff(hd,x);diff(Sum tl,x)]
	)

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

let rec apply e n =
match e with
	|X -> n
	|INT i -> i
	|ADD (e1,e2) -> apply e1 n + apply e2 n
	|SUB (e1,e2) -> apply e1 n - apply e2 n
	|MUL (e1,e2) -> apply e1 n * apply e2 n
	|DIV (e1,e2) -> apply e1 n / apply e2 n 
	|SIGMA (e1,e2,e3) -> 
	let rec sigma a b e3 =
			if( a > b+1) then raise (Failure "Error!")
			else if (a <= b)
			then (apply e3 a)+(sigma (a+1) b e3) 
			else 0 in 
			sigma (apply e1 n) (apply e2 n) e3

let calculator : exp -> int
=fun e -> apply e 1

 