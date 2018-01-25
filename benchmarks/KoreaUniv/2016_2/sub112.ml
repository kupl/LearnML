(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
	= fun lst ->
		match lst with
		| [] -> 0
		| hd::[] -> hd
		| hd::tl ->
			let num = max tl in
				if num > hd then num
				else hd;;

(* print_int (max [10; 25; 132]);;
print_endline "";;
print_int (max [1; 4; 132; 551; 32; 15]);;
print_endline "";;
print_int (max []);;
print_endline "";; *)

let rec min : int list -> int
	= fun lst ->
		match lst with
		| [] -> 0
		| hd::[] -> hd
		| hd:: tl ->
			let num = min tl in
				if num < hd then num
				else hd;;

(* print_int (min [1;2;3;4]);;
print_endline "";;
print_int (min [0; 1; 5; 3; 2]);;
print_endline "";;
print_int (min [32; 152; 3; 23; 142]);;
print_endline "";;
print_int (min []);;
print_endline "";; *)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	match lst with
	| [] -> []
	| hd::tl ->
		if pred hd = true then hd :: (filter pred tl)
		else filter pred tl;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);;

(* let inc x = x + 1;;
let mul x = x * 2;;
print_int ((double inc) 1);;
print_endline "";;
print_int (((double double) inc) 0);;
print_endline "";
print_int (((double (double double)) inc) 5);;
print_endline "";
print_int ((double mul) 1);;
print_endline "";;
print_int ((double double) mul 2);;
print_endline "";; *)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
	= fun n tree ->
		match tree with
		| Empty -> false
		| Node (num, tree1, tree2) ->
			if num = n then true
			else mem n tree1 || mem n tree2;;

(* let t1 = Node (1, Empty, Empty) *)
(* let t2 = Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty)) *)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat;;

let rec natadd : nat -> nat -> nat
	= fun n1 n2 ->
		match n1 with
		| ZERO -> n2
		| SUCC(num) -> natadd num (SUCC(n2));;


let rec natmul : nat -> nat -> nat
	= fun n1 n2 ->
		match n1 with
		| ZERO -> ZERO
		| SUCC(num) -> natadd n2 (natmul num n2);;

(* let two = SUCC(SUCC ZERO);;
let three = SUCC(SUCC (SUCC ZERO));; *)

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
	| Minus of exp * exp;;

let rec eval_exp : exp -> int
	= fun e ->
		match e with
		| Num(num) -> num
		| Plus(n1, n2) -> eval_exp(n1) + eval_exp(n2)
		| Minus(n1, n2) -> eval_exp(n1) - eval_exp(n2);;

let rec eval : formula -> bool
	= fun f ->
		match f with
		| True -> true
		| False -> false
		| Not(v) -> not (eval v)
		| AndAlso(v1, v2) -> (eval v1) && (eval v2)
		| OrElse(v1, v2) -> (eval v1) || (eval v2)
		| Imply(v1, v2) -> if v1 = False then true else (eval v2)
		| Equal(e1, e2) -> if (eval_exp e1) = (eval_exp e2) then true else false;;
(*          아래가 정답코드               *)
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
	let rec convert exp =
		match exp with
		| Num(a) -> a
		| Plus(a,b) -> (convert a) + (convert b)
		| Minus(a,b) -> (convert a) - (convert b)
			in
	match f with
	| True -> true
	| False -> false
	| Not(a) -> not (eval a)
	| AndAlso(a,b) -> (eval a) && (eval b)
	| OrElse(a,b) -> (eval a) || (eval b)
	| Imply(a,b) ->	(eval b) || not(eval a)
	| Equal(a,b) -> (convert a) = (convert b);;