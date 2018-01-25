(*********************)

let rec fold f l =
	match l with
	| [] -> failwith "Empty List!!"
	| [hd] -> hd
	| hd::tl ->  f hd (fold f tl);;

let big : int ->int -> int
= fun x y ->
 if x>y then x else y;;

let small : int -> int -> int
= fun x y ->
 if x<y then x else y;;

let max lst = fold big lst;;

let min lst = fold small lst;;

(**********************)
(*      Problem 2     *)
(**********************)

let rec filter pred lst =
	match lst with
	| [] -> []
	| hd::tl ->
		if (pred hd) then hd::(filter pred tl)
		else (filter pred tl);;

(**********************)
(*      Problem 3     *)
(**********************)

let inc x = x + 1;;

let mul x = x *2;;

let rec double f a =
	f (f a);;

(**********************)
(*      Problem 4     *)
(**********************)

type btree =
			| Empty
			| Node of int * btree * btree

 let t1 = Node (1, Empty, Empty);;
 let t2 = Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty));;

let rec mem : int -> btree -> bool
= fun n tree ->
	match tree with
	| Empty -> false
	| Node (x,left,right) ->
		n = x || (mem n left) ||(mem n right);;

(***********************)
(*      Problem 5      *)
(***********************)

type nat =
	| ZERO
	| SUCC of nat

let two = SUCC ( SUCC ZERO );;
let three = SUCC ( SUCC ( SUCC ZERO ) );;

let rec natadd : nat->nat->nat
= fun n1 n2 ->
	match n1 with
	| ZERO -> n2
	| SUCC (x) -> SUCC (natadd x n2)
	
let rec natmul : nat->nat->nat
= fun n1 n2 ->
	match n1 with
	| ZERO -> ZERO
	| SUCC (x) -> natadd(natmul x n2) n2

(***********************)
(*      Problem 6      *)
(***********************)

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

let rec cal : exp -> int
= fun c->
	match c with
	| Num i -> i
	| Plus (e1,e2) -> cal e1 + cal e2
	| Minus (e1,e2) -> cal e1 - cal e2

let rec eval : formula -> bool
= fun f ->
	match f with
	| True -> true
	| False -> false
	| Not a -> not (eval a)
	| AndAlso (a,b) -> eval a && eval b
	| OrElse (a,b) -> eval a || eval b
	|	Imply (a,b) -> not (eval a && not (eval b))
	| Equal (e1,e2) ->
		if cal e1 = cal e2 then true
		else false

		
	
