(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
=fun lst ->
	match lst with
	| [] -> min_int
  | hd::tl -> if (hd > max tl) then hd else max tl

let rec min : int list -> int
= fun lst -> (* TODO *)
match lst with
| [] -> max_int
| hd::tl -> if (hd < min tl) then hd else min tl

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = (* TODO *)
match lst with
| [] -> []
| hd::tl -> if pred hd then (hd :: filter pred tl)
											 else filter pred tl

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = (* TODO *)f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->  (* TODO *)
match tree with
| Empty -> false
| Node (a, tree1, tree2) -> 
((a=n) || (mem n tree1) || (mem n tree2))

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec make n =
match n with
| 0 -> ZERO
| _ -> SUCC( make (n-1))

let rec natcnt n =
match n with
| ZERO -> 0
| SUCC n1 -> 1 +( natcnt n1)

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)
let v1 = natcnt n1 in
let v2 = natcnt n2 in
let v3 = (v2 + v1) in (make v3)

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)
let v1 = natcnt n1 in
let v2 = natcnt n2 in
let v3 = (v2 * v1) in (make v3)

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
= fun f ->  (* TODO *)
match f with
| True -> true
| False -> false
| Not f1 ->
let v1 = eval f1 in (if v1 = true then false else true)
| AndAlso (f1, f2) ->
		let v1 = eval f1 in
		let v2 = eval f2 in (v1 && v2)
| OrElse (f1, f2) ->
		let v1 = eval f1 in
		let v2 = eval f2 in (v1 || v2)
| Imply (f1, f2) ->
		let v1 = eval f1 in
		let v2 = eval f2 in
		(match v1, v2 with
			|true,false -> false
			|_ -> true)
|Equal (e1, e2) ->
let rec ev : exp -> int
= fun a -> 
match a with
| Num c -> c
| Plus (c1, c2) -> ((ev c1) + (ev c2))
| Minus (c1, c2) -> ((ev c1) - (ev c2)) in
let v1 = ev e1 in let v2 = ev e2 in
(if v1 = v2 then true else false)



