(*********************)
(*     Problem 1     *)
(*********************)
let rec num_max a b =
if a>b then a
else b

let rec max : int list -> int
=fun lst -> match lst with
| [] -> raise (Failure "list is too short")
| [x] -> x
| x :: tl -> num_max x (max tl)


let rec num_min a b =
if a<b then a
else b

let rec min : int list -> int
= fun lst -> match lst with
| [] -> raise (Failure "list is too short")
| [x] -> x
| x :: tl -> num_min x (min tl)


(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
match lst with
| [] -> []
| hd::tl -> if pred hd = true then [hd] @ (filter pred tl)
else filter pred tl


(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =
f (f a)


(*********************)
(*     Problem 4     *)
(*********************)
type btree =
| Empty
| Node of int * btree * btree	
	
	
let rec mem : int -> btree -> bool 
= fun n tree -> match tree with
|Empty -> false
|Node (x, y, z) -> if x = n then true 
else mem n y || mem n z


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> n2 
| SUCC tl ->  natadd tl (SUCC (n2)) 

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC ZERO -> n2
| SUCC tl ->  natadd n2 (natmul tl n2)

(*********************)
(*     Problem 6     *)
(*********************)
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

let rec cal_exp n =
match n with
| Num a -> a
| Plus (e1, e2) -> (cal_exp e1 + cal_exp e2)
| Minus (e1, e2) -> (cal_exp e1 - cal_exp e2)

let rec eval : formula -> bool
= fun f -> match f with
| True -> true
| False -> false
| Not x -> 
	let v = eval x in
	(match v with
	| true -> eval False
	| false -> eval True)
| AndAlso (e1,e2) ->
	let v1 = eval e1 in
	let v2 = eval e2 in
		(match v1,v2 with
		| true,true -> eval True
		| true,false -> eval False
		| false,true -> eval False
		| false,false -> eval False)
| OrElse (e1,e2) ->
	let v1 = eval e1 in
	let v2 = eval e2 in
		(match v1,v2 with
		| true,true -> eval True
		| true,false -> eval True
		| false,true -> eval True
		| false,false -> eval False)
| Imply (e1,e2) ->
	let v1 = eval e1 in
	let v2 = eval e2 in
		(match v1,v2 with
		| true,true -> eval True
		| true,false -> eval False
		| false,true -> eval True
		| false,false -> eval True)
| Equal (e1,e2) -> 
	let v1 = cal_exp e1 in
	let v2 = cal_exp e2 in
	(match v1,v2 with
	| v1,v2 -> if v1 = v2 then eval True else eval False )