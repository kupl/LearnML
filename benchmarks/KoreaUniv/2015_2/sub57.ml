(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
	match lst with
	|[]->[]
	|(a::b) ->
		if pred a then a :: filter pred b
		else filter pred b

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun(l1, l2) 
	-> match l1 with 
	|[]-> l2	
	|hd1::tl1
		-> (match l2 with 
		|[]->l1	
		|hd2::tl2 ->if hd1 < hd2 then [hd1]@[hd2]@(zipper(tl1,tl2))	
			else [hd2]@[hd1]@(zipper(tl1,tl2))
			)

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f)->
	if n = 0 then (fun x-> x)	
	else (fun x-> f(iter(n-1,f) x))

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
	|Const n -> Const 0
	|Var y -> if y = x then Const 1 else Const 0
	|Power(z, n) -> if z = x then	
				(if n = 0 then Const 0
				else if n = 1 then Const 1
				else Times[Const n; Power(z, n-1)])
			else Const 0
	|Times l1 -> 
		(match l1 with 
		|[] -> Const 0
		|h::[] -> diff (h, x)
		|h::t ->
			(match h with
			|Const n -> Times[Const n; diff(Times t, x)]
			|_ -> Sum[ Times [diff(h,x); Times t]; Times [h; diff(Times t, x)]  ] 
			)
		)
	|Sum l1 -> 	
		(match l1 with	
		|[] -> Const 0
		|h::t -> Sum[diff(h,x);diff(Sum t, x)]
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
let calculator : exp -> int
=fun e -> 0 (* TODO *)