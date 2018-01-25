(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l=
match l with
|[] -> 0
|hd :: [] -> hd
|hd :: tl -> f hd (fold f tl);;

let rec max : int list -> int
= fun lst -> fold(fun x y -> if(x>y) then x else y) lst;;

let rec min : int list -> int
= fun lst ->  fold(fun x y -> if(x>y) then y else x) lst;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
match lst with
|[]->[]
|hd :: tl -> if(pred hd) then hd :: filter pred tl else filter pred tl;;


(*********************)
(*     Problem 3     *)
(*********************)
let inc x = x + 1;;
let mul x = x * 2;;

let rec double f a = f (f a);;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree;;

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
| Empty -> false
| Node (x, t1, t2) -> if(n == x) then true else mem n t1 || mem n t2;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC (n) -> natadd n (SUCC (n2));;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC (n) -> natadd (natmul n n2) n2;;

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
	| Minus of exp * exp ;;

let rec cal x =
match x with
|Num n -> n
|Plus (n1, n2) -> (cal n1) + (cal n2)
|Minus (n1, n2) -> (cal n1) - (cal n2);;
	
	
let rec eval : formula -> bool
= fun f -> match f with
|True -> true
|False -> false
|Not (n) -> if (eval n) then false else true
|AndAlso (n1, n2) -> (eval n1) && (eval n2)
|OrElse (n1, n2) -> (eval n1) || (eval n2)
|Imply (n1, n2) -> if((eval n1) && not(eval n2)) then false else true
|Equal (n1, n2) -> 
	let r1 = cal n1 in
	let r2 = cal n2 in
	if(r1 == r2) then true else false;;

