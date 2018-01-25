let rec fold f l a =
	match l with
	[] -> a
	| hd::tl -> f hd (fold f tl a)

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> fold (fun n1 n2 -> if n1 > n2 then n1 else n2) lst min_int (* TODO *)

let rec min : int list -> int
= fun lst -> fold (fun n1 n2 -> if n1 < n2 then n1 else n2) lst max_int (* TODO *)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
	[] -> []
	|hd::tl -> if (pred hd) then hd::(filter pred tl) else (filter pred tl) (* TODO *)

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a) (* TODO *)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
	Empty -> false
	|Node(a,left,right) -> (mem n left)||(a = n)||(mem n right) (* TODO *)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	ZERO -> n2
	|SUCC(sub_n) -> SUCC(natadd sub_n n2) (* TODO *)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	ZERO -> ZERO
	|SUCC(sub_n) -> natadd (natmul sub_n n2) (n2) (* TODO *)

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
= fun f -> let rec calcExp e = (match e with
	Num(a) -> a
	|Plus(sub1,sub2) -> (calcExp sub1) + (calcExp sub2)
	|Minus(sub1,sub2) -> (calcExp sub1) - (calcExp sub2)) in (match f with
	True -> true
	|False -> false
	|Not(sub_f) -> not (eval sub_f)
	|AndAlso(f1,f2) -> (eval f1) && (eval f2)
	|OrElse(f1,f2) -> (eval f1) || (eval f2)
	|Imply(f1,f2) -> not ((eval f1) && (not (eval f2)))
	|Equal(e1,e2) -> (calcExp e1) = (calcExp e2)) (* TODO *)

