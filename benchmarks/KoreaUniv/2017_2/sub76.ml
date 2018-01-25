(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
|Empty -> Empty
|Node (i, b1, b2) -> Node (i, mirror b2, mirror b1)

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natlength n1 =
	match n1 with
	ZERO -> 0
	|SUCC n1' -> 1 + (natlength n1')

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	match n1 with
	ZERO -> n2
	|SUCC n1' -> SUCC (natadd n1' n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
	ZERO -> ZERO
	|SUCC n1' -> natadd (natmul n1 n1') n1 
(*
let rec natexp : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
	ZERO -> 
	|SUCC n2' -> natadd natexp (natmul n1 n1) n2'  
*)
(* problem*)
(*type formula = 
	True
	|False
	|Var of string
	|Neg of formula
	|And of formula * formula
	|Or of formula * formula
	|Imply of formula * formula
	|Iff of formula * formula

let sat : formula -> bool
= fun f ->
	match f with
	True -> true
	|False -> false
	*)

(* problem 5*)
(*type exp = X
					|INT of int
					|ADD of exp * exp
					|SUB of exp * exp
					|MUL of exp * exp
					|DIV of exp * exp
					|SIGMA of exp * exp * exp

let rec calculator : exp -> int
=fun e ->
match e with
	INT n -> n
	|ADD (x, y) -> calculator x + calculator y
	|SUB (x, y) -> calculator x - calculator y
	|MUL (x, y) -> calculator x * calculator y
	|DIV (x, y) -> calculator x / calculator y
	|SIGMA (x, y, z) ->
		let x1 = calculator x in
		let y1 = calculator y in
		(match x1 with
		|INT n1 
		y1
*)
(* problem 6*)
(* problem 7*)

