(* Prob 1 *)
let rec max l =
	match l with
	| [] -> 0
	| [a] -> a
	| hd::tl -> 
		if hd > (max tl) then hd else max tl

let rec min l =
	match l with
	| [] -> 0
	| [a] -> a
	| hd::tl -> 
		if hd < (min tl) then hd else min tl

(* Prob 2 *)
let rec filter f l =
	match l with
	| [] -> []
	| hd::tl ->
		if f(hd) = true then [hd] @ filter f tl
		else filter f tl

(* Prob 3 *)
let double f(a) =	f (f a)

(* Prob 4 *)
type btree = Empty | Node of int * btree * btree

let rec mem a t =
	match t with
	| Empty -> false
	| Node(q, p, r) -> 
		if q = a then true
		else mem a p || mem a r

(* Prob 5 *)

type nat = ZERO | SUCC of nat

let rec natadd a b =
	match b with
	| ZERO -> a
	| SUCC(x) -> SUCC(natadd a x)

let rec natmul a b =
	if a = ZERO || b = ZERO then ZERO
	else natadd b b 

(* Prob 6 *)

type formula = 
| True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp =
| Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec expfun e =
	match e with
	| Num(n) -> n
	| Plus(e1, e2) -> expfun e1 + expfun e2
	| Minus(e1, e2) -> expfun e1 - expfun e2

let rec eval f =
	match f with
	| True -> true
	| False -> false
	| Not(f1) -> 
		if f1 = True then false
		else true
	| AndAlso(f1, f2) ->
		if f1 = True && f2 = True then true	else false
	| OrElse(f1, f2) ->
		if f1 = True || f2 = True then true	else false
	| Imply(f1, f2) ->
		if f1 = True && f2 = False then false	else true
	| Equal(e1, e2) ->
		if expfun e1 = expfun e2 then true else false
