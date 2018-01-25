(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 0 (* TODO *)

let rec max : int list -> int
= fun lst -> match lst with [] -> min_int | h::t -> let s = max t in if (s > h) then s else h

let rec min : int list -> int
= fun lst -> 0 (* TODO *)

let rec min : int list -> int
= fun lst -> match lst with [] -> max_int | h::t -> let s = min t in if (s > h) then h else s

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = [] (* TODO *)

let rec filter pred lst = match lst with [] -> [] | h::t -> if (pred h) then h::(filter pred t) else (filter pred t)

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = a (* TODO *)

let rec double f a = f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> true (* TODO *)

let rec mem : int -> btree -> bool
 = fun n tree -> match tree with Empty -> false | Node(a, b, c) -> if a = n then true else ((mem n b) || (mem n c))

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> ZERO (* TODO *)

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with ZERO -> n2 | SUCC (a) -> SUCC (natadd a n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> ZERO (* TODO *)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with ZERO -> ZERO | SUCC (a) -> natadd n1 (natmul n1 a)


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
= fun f -> true (* TODO *)

let rec eval : formula -> bool
= fun f -> match f with
True -> true
| False -> false
| Not(a) -> if eval a = true then false else true
| AndAlso(a, b) -> if eval a = true && eval b = true then true else false
| OrElse(a, b) -> if eval a = true || eval b = true then true else false
| Imply(a, b) -> if eval a = false || eval b = true then true else false
| Equal(a, b) ->
let rec res p = match p with
| Plus(x, y) -> res(x) + res(y)
| Minus(x, y) -> res(x) - res(y)
| Num x -> x
in if res(a) = res(b) then true else false
