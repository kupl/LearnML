(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a = 
match l with
	| [] -> a
  	| [x] -> x
  	| hd::tl -> f hd (fold f tl a)

let compareMax : int -> int -> int
= fun a b -> if a > b then a else b

let rec max : int list -> int
= fun lst -> fold compareMax lst 0

let compareMin : int -> int -> int
= fun a b -> if a < b then a else b

let rec min : int list -> int
= fun lst -> fold compareMin lst 0

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
  	| [] -> []
  	| [a] -> if pred a then [a] else []
  	| hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl


(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f ( f a )

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
  	| Empty -> false
  	| Node (a, Empty, x) | Node (a, x , Empty) -> if n = a then true else mem n x
  	| Node (a, b, c) -> if n = a then true else if mem n b then true else mem n c ;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
	| ZERO -> n1
	| SUCC (t) -> natadd (SUCC(n1)) t;;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
	| ZERO -> ZERO
	| SUCC ZERO -> n1
	| SUCC (t) -> natadd n1 (natmul n1 t);;

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

let andAlso : bool * bool -> bool
= fun f -> match f with
  	| (true, true) -> true
  	| _ -> false

let orElse : bool * bool -> bool
= fun f -> match f with
	| (false, false) -> false
	| _ -> true

let imply : bool * bool -> bool
= fun f -> match f with
	| (frue, false) -> false
	| _ -> true

let rec ca : exp -> int
= fun f -> match f with
	| Num a -> a
	| Plus (a, b) -> (ca a) + (ca b)
	| Minus (a, b) -> (ca a) - (ca b)


let equal : exp * exp -> bool
= fun f -> match f with
	|(a,b) -> if ca a = ca b then true else false





let rec eval : formula -> bool
= fun f -> match f with
	| True -> true
	| False -> false
	| AndAlso (a,b) -> andAlso (eval a, eval b)
	| OrElse (a,b) -> orElse (eval a, eval b)
	| Imply (a,b) -> imply (eval a, eval b)
	| Equal (a,b) -> equal (a, b)
	| _ -> raise (Failure "eval requires formula") 

