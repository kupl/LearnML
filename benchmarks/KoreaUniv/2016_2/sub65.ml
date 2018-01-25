(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f lst
= match lst with
|[a] -> a
|hd::tl -> f hd (fold f tl)

let rec max : int list -> int
= fun lst -> fold (fun x y -> if x >= y then x else y) lst

let rec min : int list -> int
= fun lst -> fold (fun x y -> if x <= y then x else y) lst

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
|[] -> []
|hd::tl -> if (pred hd) then [hd]@ filter pred tl else filter pred tl

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
= fun n tree -> match tree with
|Empty -> false
|Node (x, y, z) -> if x = n then true else (mem n y) || (mem n z)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec nat_to_int: nat -> int
= fun n -> match n with
|ZERO -> 0
|SUCC x -> 1 + (nat_to_int x)

let rec int_to_nat: int -> nat
= fun c -> match c with
|0 -> ZERO
|x -> SUCC (int_to_nat (x-1))

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	let c1 = nat_to_int n1 and c2 = nat_to_int n2 in
		int_to_nat (c1 + c2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	let c1 = nat_to_int n1 and c2 =nat_to_int n2 in
		int_to_nat (c1 * c2)

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


let rec exp_to_int : exp -> int
= fun e ->
	match e with
	| Num n -> n
	| Plus (n1, n2) -> exp_to_int n1 + exp_to_int n2
	| Minus (n1, n2) -> exp_to_int n1 - exp_to_int n2

let rec eval : formula -> bool
= fun f -> 
	match f with
	| True -> true
	| False -> false
	| Not f1 -> not (eval f1)
	| AndAlso (f1, f2) -> eval f1 && eval f2
	| OrElse (f1, f2) -> eval f1 || eval f2
	| Imply (f1, f2) -> 
		(match f1, f2 with
			|True, False -> false
			|_ -> true)
	|Equal (e1, e2) -> exp_to_int e1 = exp_to_int e2
