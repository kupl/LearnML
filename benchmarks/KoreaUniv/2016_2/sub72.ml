exception Problem


let rec fold f l a = 
		match  l with
		[]-> a
		|hd::tl -> f hd (fold f tl a)
(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
	[]-> raise Problem
	|hd::tl -> fold(fun x y -> if x>=y then x else y) lst hd

let rec min : int list -> int
= fun lst -> match lst with
	[]-> raise Problem
	|hd::tl -> fold (fun x y -> if x<=y then x else y) lst hd

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	match lst with
		[]-> []
		|hd::tl-> fold(fun x y -> if (pred x) then [x]@y else y) lst []

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = 
			f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
		match tree with
		Empty -> false
		|Node (a,b,c) -> if a=n then true else (mem n b || mem n c);;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
		match n1 with
		ZERO-> (n2)
		|SUCC nat -> SUCC (natadd nat n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
		match n1 with
		ZERO-> ZERO
		|SUCC nat -> natadd n2 (natmul nat n2)

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

let rec cal num = match num with
	Num  i -> Num i
	|Plus (a,b)-> (match a, b with
									 Num a2, Num b2 -> Num(a2 + b2)
									|_,_ -> Plus(cal a, cal b))													
	|Minus (a,b) -> (match a, b with
									Num a2, Num b2 -> Num (a2 - b2)
									|_,_ -> Minus(cal a, cal b)) 


let rec eval : formula -> bool
= fun f -> match f with
	True-> true
	|False-> false
	|Not x -> if (eval(x) = true) then false else true
	|AndAlso (x,y) -> (eval x) && (eval y)
	|OrElse (x,y) -> (eval x) || (eval y)
	|Imply (x,y) -> if((eval x = true) &&(eval y = false)) then false else true
	|Equal (x,y) ->(match x, y with
											Num x2, Num y2-> if x2 =y2 then true else false
											|_,_ -> eval (Equal (cal x, cal y)))


		



