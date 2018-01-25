(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> let retnmax : int -> int -> int = fun a b -> if a>b then a else b in match lst with [a] -> a | hd::tl -> retnmax hd (max tl);;

let rec min : int list -> int
= fun lst -> let retnmin : int -> int -> int = fun a b -> if a<b then a else b in match lst with [a] -> a | hd::tl -> retnmin hd (min tl);;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with [] -> [] | hd::tl -> if pred hd then hd::(filter pred tl) else (filter pred tl);;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with | Empty -> false | Node(a, Empty, Empty) -> if a==n then true else false | Node(a, b, Empty) -> if a==n then true else if mem n b then true else false | Node(a, Empty, c) -> if a==n then true else if mem n c then true else false | Node(a, b, c) -> if a==n then true else if mem n b || mem n c then true else false;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with ZERO -> n2 | SUCC(n0) -> natadd n0 (SUCC(n2));;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> let rec overload : nat -> nat -> nat -> nat = fun n1 n2 k -> match n1 with ZERO -> ZERO | SUCC(ZERO) -> n2 | SUCC(n0) -> overload n0 (natadd n2 k ) k in overload n1 n2 n2;;

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
= let rec expcall : exp -> int = fun e -> match e with Num(x) -> x | Plus(x, y) -> (expcall(x)) + (expcall(y)) | Minus(x, y) -> (expcall(x)) - (expcall(y)) in fun f -> match f with True -> true | False -> false | Not(x) -> not (eval(x)) | AndAlso(x, y) -> (eval(x))&&(eval(y)) | OrElse(x, y) -> (eval(x))||(eval(y)) | Imply(x, y) -> if (eval(x)) == true && (eval(y)) == false then false else true | Equal(x, y) -> if (expcall(x)) == (expcall(y)) then true else false;;

