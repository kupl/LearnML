(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = [];;
let rec filter p l =
	match l with
	[] -> []
| h::t -> if (p h) then h::(filter p t) else (filter p t);;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> [];;

let rec zipper : int list * int list -> int list
=fun (l1,l2) ->
	match (l1,l2) with
| (_,[]) -> l1
| ([],_) -> l2
| (h1::t1,h2::t2) -> [h1; h2]@(zipper (t1,t2));;
 
(*******************)
(* Problem 3: iter *)
(*******************)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> f;;

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
	match n with
	0 -> (fun x->x)
| _ -> (fun x -> iter (n-1,f) (f x));;

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list;;

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> aexp;;

let rec diff : aexp * string -> aexp
= fun (aexp,x) ->
	match aexp with
	Const n -> Const 0
| Var y -> if y=x then Const 1 else Const 0
| Power (s,n) ->
	(if s=x then
		(match n with
		0 -> Const 0
	| 1 -> Const 1
	| 2 -> Times [Const 2; Var s]
	| _ -> Times [Const n; Power (s,n-1)])
	else Const 0)
| Times l ->
	(match l with
		[] -> Const 1
	| h::[] -> (diff(h,x))
	| h::t ->
		(match h with
			Const 1 -> (diff(Times t,x))
		| Const n -> (Times [Const n; diff(Times t,x)])
		| _ -> (Sum [Times (diff(h,x)::t); Times [h; diff(Times t,x)]])))
| Sum l ->
	(match l with
		[] -> Const 0
	| h::[] -> (diff(h,x))
	| h::t ->  (Sum [diff(h,x); diff(Sum t,x)]));;

(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp;;

let calculator : exp -> int
=fun e -> 0;;

let rec e_to_f : exp -> (int -> int)
= fun e ->
	match e with
| X -> (fun x -> x)
| INT n -> (fun x -> n)
| ADD (e1,e2) -> (fun x -> (((e_to_f e1) x) + ((e_to_f e2) x)))
| SUB (e1,e2) -> (fun x -> (((e_to_f e1) x) - ((e_to_f e2) x)))
| MUL (e1,e2) -> (fun x -> (((e_to_f e1) x) * ((e_to_f e2) x)))
| DIV (e1,e2) -> (fun x -> (((e_to_f e1) x) / ((e_to_f e2) x)))
| _ -> (fun x -> 0);;

let rec sigma : int * int * (int -> int) -> int
= fun (n1,n2,f) ->
	if n1=n2 then f n1
		else (f n1) + (sigma (n1+1,n2,f));;

let rec reculse : exp -> int
= fun e ->
match e with
	X -> raise (Failure "fail!!")
| INT n -> n
| ADD (INT n1,INT n2) -> n1+n2
| ADD (e1, e2) -> reculse e1 + reculse e2
| SUB (INT n1,INT n2) -> n1-n2
| SUB (e1, e2) -> reculse e1 - reculse e2
| MUL (INT n1,INT n2) -> n1*n2
| MUL (e1, e2) -> reculse e1 * reculse e2
| DIV (INT n1,INT n2) -> n1/n2
| DIV (e1, e2) -> reculse e1 / reculse e2
| SIGMA (INT n1, INT n2, e) -> sigma (n1 ,n2, (e_to_f e))
| SIGMA (e1, e2, e3) -> sigma (reculse e1, reculse e2, (e_to_f e3));;

let calculator : exp -> int
= fun e ->
	(match e with
	X -> 0
| INT n -> n
| ADD (e1,e2) -> (reculse e1 + reculse e2)
| SUB (e1,e2) -> (reculse e1 - reculse e2)
| MUL (e1,e2) -> (reculse e1 * reculse e2)
| DIV (e1,e2) -> (reculse e1 / reculse e2)
| SIGMA (e1,e2,e3) -> sigma (reculse e1, reculse e2, (e_to_f e3)));;

