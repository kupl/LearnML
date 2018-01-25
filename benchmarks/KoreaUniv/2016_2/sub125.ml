(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->  (* TODO *)
	match lst with
	| [] -> min_int
	| h::t ->  if h > (max t) then h else (max t)

let rec min : int list -> int
= fun lst -> (* TODO *)
	match lst with
	| [] -> max_int
	| h::t ->  if h < (min t) then h else (min t)
(*********************)
(*     Problem 2     *)
(*********************)

let rec filter pred lst =
(* TODO *)
	match lst with
	|[] -> []
	|h::t -> if pred h then h::filter pred t else filter pred t
	
(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = 
	f (f a) (* TODO *)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
match tree with
|Empty -> false
|Node(a,b,c) -> if a==n then true else mem n b || mem n c


 (* TODO *)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
(* TODO *)
	match n2 with
	|ZERO -> n1
	|(SUCC p) -> if p == ZERO then (SUCC n1) else natadd (SUCC n1) p

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
	match n2 with
	|ZERO -> ZERO
	|(SUCC p) -> if p == ZERO then n1 else natadd (natmul n1 p) n1
let two = SUCC (SUCC ZERO)
let three = SUCC (SUCC (SUCC ZERO))
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

let rec exptoint : exp -> int
= fun e -> 
	match e with
	|Num a -> a
	|Plus (a,b) -> exptoint a + exptoint b
	|Minus (a,b) -> exptoint a - exptoint b

let rec eval : formula -> bool
= fun f ->  (* TODO *)
match f with
|True -> true
|False -> false
|Not(f1) -> if f1 == True then false else true
|AndAlso(f1,f2) -> if f1 == True && f2 == True then true else false
|OrElse(f1,f2) -> if AndAlso(f1,f2) == False then false else true
|Imply(f1,f2) -> if Not(f1) == True || AndAlso(f1,f2) == True then true else false
|Equal(e1, e2) -> if exptoint e1 == exptoint e2 then true else false




