(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with 
						| [] -> min_int
						| hd::tl -> if (hd>(max tl)) then hd else (max tl)

let rec min : int list -> int
= fun lst -> match lst with
						| [] -> max_int
						| hd::tl -> if(hd<(max tl)) then hd else (max tl)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	match lst with
	| [] -> []
	| hd::tl -> if(pred hd) then hd::(filter pred tl)  else (filter pred tl)

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =	f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
	| Empty -> false
	| Node(x, t1, t2) -> (x=n) || (mem n t1) || (mem n t2)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	| ZERO -> n2
	| SUCC(v) -> (natadd v (SUCC(n2)))

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	| ZERO -> ZERO
	| SUCC(v) -> (natadd n2 (natmul v n2))

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

let rec eval : formula -> bool
= fun f -> match f with
	| True -> true
	| False -> false
	| Not v ->
		let e =  eval v in
			 if (e=true) then false else true
	| AndAlso (v1,v2) ->
		let e1 = eval v1 in
		let e2 = eval v2 in
			begin
			 if(e1=true&&e2=true) then true else false
			end
	|OrElse (v1,v2) ->
		let e1 = eval v1 in
		let e2 = eval v2 in
			begin
			 if(e1=false&&e2=false) then false else true
			end
	|Imply (v1,v2) ->
		let e1 = eval v1 in
		let e2 = eval v2 in
			begin
			 if(e1=true&&e2=false) then false else true
			end
	|Equal (v1,v2) ->
		let rec eval2 : exp -> int
		=fun g -> match g with
			| Num s -> s
			| Plus (s1,s2) ->
				 (eval2 s1)+(eval2 s2)
			| Minus (s1,s2) ->
				 (eval2 s1)-(eval2 s2) in
		let e1 = eval2 v1 in
		let e2 = eval2 v2 in
			begin
			 if(e1=e2) then true else false
			end


