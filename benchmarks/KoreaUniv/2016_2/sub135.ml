(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
	match l with
	 | [] -> a
	 | hd::tl -> f hd (fold f tl a)

let rec max : int list -> int
= fun lst -> match lst with
			| [] -> raise(Failure "empty list")
			| hd::tl -> fold (fun x y -> if x>y then x else y) tl hd

let rec min : int list -> int
= fun lst -> match lst with
			| [] -> raise(Failure "empty list!")
			| hd::tl -> fold (fun x y -> if x<y then x else y) tl hd 

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
			| [] -> []
			| hd::tl -> if pred(hd) = true then hd::(filter pred tl)
						else filter pred tl

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f(f(a))

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
			| Empty -> false
			| Node(a,b,c) -> if a = n then true
							else (mem n b) || (mem n c)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
			| ZERO -> n2
			| SUCC(a) -> SUCC(natadd a n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
		    | ZERO -> ZERO
			| SUCC(a) -> natadd n2(natmul a n2)
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

let rec evall : exp -> int
= fun e -> match e with
		| Num(a) -> a
		| Plus(a,b) -> evall(a)+evall(b)
		| Minus(a,b) -> evall(a)-evall(b)

let rec eval : formula -> bool
= fun f -> match f with
		| True -> true
		| False -> false
		| Not(a) -> if eval(a) then false else true
		| AndAlso(a,b) -> eval(a) && eval(b)
		| OrElse(a,b) -> eval(a) || eval(b)
		| Imply(a,b) -> eval(Not(AndAlso(a,Not(b))))
		| Equal(a,b) -> if evall(a)=evall(b) then true else false
