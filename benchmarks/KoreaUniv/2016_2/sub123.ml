(* 2013210062 jihwanlee *)
(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a);;

let rec max : int list -> int
= fun lst -> 
	match lst with
	| [] -> raise (Failure "Exception : Empty List")
	| hd::tl -> fold (fun x y -> if x > y then x else y) lst hd;;

let rec min : int list -> int
= fun lst -> 
	match lst with
	| [] -> raise (Failure "Exception : Empty List")
	| hd::tl -> fold (fun x y -> if x < y then x else y) lst hd;;




(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	match lst with
	| [] -> []
	| hd::tl -> if (pred hd = false) then filter pred tl else hd::(filter pred tl);;



(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);; (* TODO *)




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
	|Node (num, bt1, bt2) -> if n = num then true else (mem num bt1 || mem num bt2);;
	
(* let t1 = Node(1,Empty,Empty);;
let t2 = Node(1, Node(2,Empty,Empty),Node(3,Empty,Empty));; *)



(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	match n1 with
	| ZERO -> n2
	| SUCC (a) -> SUCC(natadd a n2);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	match n2 with
	| ZERO -> ZERO 
	| SUCC (a) -> natadd n1 (natmul n1 a);;





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
= fun f -> 
	let rec evalexp :  exp -> int = 
	fun f1 -> match f1 with
		| Num (a) -> a
		| Plus (a,b) -> evalexp (a) + evalexp (b)
		| Minus (a,b) -> evalexp (a) - evalexp (b) 
		in

		match f with
		| True -> true
		| False -> false
		| Not f1 -> if (f1=True) then false else true
		| AndAlso (f1,f2) -> if (f1=False) then false else if (f2=True) then true else false
		| OrElse (f1,f2) -> if (f1=True) then true else if (f2=True) then true else false
		| Imply (f1,f2) -> if (f1=True && f2=False) then false else true
		| Equal (a,b) -> if (evalexp a)=(evalexp b) then true else false;;

(* eval (Imply (Imply (True, False), True));;
eval (Equal (Num 1, Plus(Num 1, Num 2)));;
eval (Equal Plus(2,3), Plus(3,2));; *)


