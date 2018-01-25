(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a)

let rec max : int list -> int
= fun lst ->
	fold (fun x y -> if x >= y then x else y) lst (-999)

let rec min : int list -> int
= fun lst ->
	fold (fun x y -> if x <= y then x else y) lst 999

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
	fold (fun x l -> if pred x then x::l else l) lst []

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
	match tree with
	| Empty -> false
	| Node (a, l1, l2) -> if a = n then true else mem n l1||mem n l2 


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec count n =
	match n with
	| ZERO -> 0 
	| SUCC (n1) -> 1 + count n1

let rec make : int -> nat -> nat 
= fun n1 n2 -> if n1 = 0 then n2 else let n2 = SUCC (n2) in make (n1-1) n2

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
	| ZERO -> n1
	| SUCC (n) -> let n1 = SUCC (n1) in natadd n1 n

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	let n11 = count n1 in
		let n22 = count n2 in
			natadd ZERO (make (n11*n22) ZERO)

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

let rec e2i e =
	match e with
	| Num (n) -> n
	| Plus (e1, e2) -> (e2i e1) + (e2i e2)
	| Minus (e3, e4) -> (e2i e3) - (e2i e4) 

let rec eval : formula -> bool
= fun f ->
	match f with
	| True -> true
	| False -> false
	| Not (f1) -> if (eval f1) = true then false else true
	| AndAlso (f2, f3) -> (eval f2)&&(eval f3)
	| OrElse (f4, f5) -> (eval f4)||(eval f5)
	| Imply (f6, f7) ->
		begin
		match (eval f6,eval f7) with
		| (true, false) -> false
		| _ -> true
		end
	| Equal (e1,e2) -> 
		let v1 = (e2i e1) in
		let v2 = (e2i e2) in if v1 = v2 then true else false
