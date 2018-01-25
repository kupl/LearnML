(*********************)
(*     Problem 1     *)
(*********************)
let rec max2 : int -> int -> int
= fun a b -> if a>=b then a else b

let rec min2 : int -> int -> int
= fun a b -> if a>=b then b else a

let rec max : int list -> int
= fun lst -> match lst with
| [] -> -999999999
| hd::tl -> max2 hd (max tl)

let rec min : int list -> int
= fun lst -> match lst with
| [] -> 999999999
| hd::tl -> min2 hd (min tl)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter : ('a -> bool) -> 'a list -> 'a list
= fun pred lst -> match lst with
| [] -> []
| hd::tl -> if (pred hd) then [hd;]@(filter pred tl) else (filter pred tl)
(* let rec filter pred lst = [] *)

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
| Empty -> false
| Node (k, leftchild, rightchild) -> if k=n then true else (mem n leftchild) || (mem n rightchild)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec int_to_nat : int -> nat
= fun n -> match n with
| 0 -> ZERO
| _ -> SUCC (int_to_nat(n-1))

let rec nat_to_int : nat -> int
= fun n -> match n with
| ZERO -> 0
| SUCC n_ -> 1 + (nat_to_int n_)

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> int_to_nat ((nat_to_int n1) + (nat_to_int n2))

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> int_to_nat ((nat_to_int n1) * (nat_to_int n2))

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
= fun ex -> match ex with
| Num (n) -> n
| Plus (n1, n2) -> exp_to_int(n1) + exp_to_int(n2)
| Minus (n1, n2) -> exp_to_int(n1) - exp_to_int(n2)

let rec eval : formula -> bool
= fun f -> match f with
| True -> true
| False -> false
| Not (a) -> not (eval a)
| AndAlso (a, b) -> if (eval a) then (eval b) else false
| OrElse (a, b) -> if (eval a) then true else (eval b)
| Imply (a, b) -> if (eval a) then (eval b) else true
| Equal (a, b) -> if exp_to_int(a) = exp_to_int(b) then true else false
