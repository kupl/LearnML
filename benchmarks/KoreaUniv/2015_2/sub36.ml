(* 2012210066 hw2.ml
	  	 컴퓨터학과 조현상 *)

(* Problem 1: filter *)
let rec filter pred lst = [];;
let rec filter f l = 
match l with
| [] -> []
| h::t -> if f h then h::filter f t else filter f t;;

(* Problem 2: zipper *)
let rec zipper : int list * int list -> int list
=fun (a,b) -> [];;
let rec zipper : int list * int list -> int list 
= fun (l1,l2) ->
match (l1,l2) with
| ([],[]) -> []
| (l1,[]) -> l1
| ([],l2) -> l2
| (h1::t1,h2::t2) -> if h1>h2 then h2::zipper (l1,t2) else h1::zipper (t1,l2);;

(* problem 3: iter *)
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> f;;
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
	if n = 0 then (fun x->x)
	else (fun x-> iter (n-1,f) (f x));;

(* Problem 4: diff *)
type aexp =
| Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> aexp;;

let rec diff : aexp * string -> aexp
= fun (aexp,x) ->
	match aexp with
| Const n -> Const 0
| Var x -> if x=x then Const 1 else Const 0
| Power (x,n) -> if x=x then
										 if n=0 then Const 0
										 else if n=1 then Const 1
										 else if n=2 then Times [Const 2; Var x]
										 else Times [Const n; Power (x,n-1)]
						 		 else Const 0
| Times lst -> 
				(match lst with
				| [] -> Const 0
				| h::[]-> diff (h,x)
				| h::t -> match h with 
								| Const 1 -> diff (Times t,x)
								| Const n -> Times [h;diff (Times t,x)]
								|_ ->  Sum[Times (diff (h,x)::t); Times [h;diff (Times t,x)]])
| Sum lst2 -> 
				(match lst2 with
				| [] -> Const 0
			  | h::[] -> (diff(h,x))
 				| h::t -> Sum[(diff (h,x));(diff ((Sum t),x))]);;

(* Problem 5 Calculator *)
type exp = X
					| INT of int
					| ADD of exp*exp
					| SUB of exp*exp
					| MUL of exp*exp
					| DIV of exp*exp
					| SIGMA of exp*exp*exp
let rec cal : exp -> int
=fun exp -> 0;;
exception FreeVariable
let rec cal (exp) =
		match exp with
		| X -> cal X
		| INT i -> i
		| ADD (e1,e2)->
				(match (e1,e2) with
					| (INT x,INT y) ->  x + y
					| (_,_) -> (cal (e1) + cal (e2)))
		| SUB (e1,e2)->
				(match (e1,e2) with
				  | (INT x,INT y) -> x - y
          | (_,_) -> (cal (e1)-cal (e2)))
		| MUL (e1,e2)->
			  (match (e1,e2) with
					| (INT x,INT y) -> x * y
					| (_,_) -> (cal (e1)*cal (e2)))
		| DIV (e1,e2)->
				(match (e1,e2) with
          | (INT x,INT y) -> x / y
          | (_,_) -> (cal (e1)/cal (e2)))
	  | SIGMA (e1,e2,e3) ->
					let x = cal e1 and y = cal e2 in(
					if x>y then 0
					else if x=y then cal e3
					else cal e3 + cal ((SIGMA (INT (x+1),INT y,e3))));;
let calcualtor e =
		cal e;;
