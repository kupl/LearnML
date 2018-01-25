(* problem 1*)
type btree = Empty | Node of int * btree * btree
let rec mirror : btree->btree
= fun f -> match f with Empty -> Empty | Node(a,b,c) -> Node(a,c,b)

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with ZERO -> n2 | SUCC c -> SUCC (natadd c n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with ZERO -> ZERO | SUCC c -> natadd n2 (natmul c n2)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with ZERO -> SUCC ZERO | SUCC c -> natmul n1 (natexp n1 c)


(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec  sat : formula -> bool
= fun f -> match f with False -> false | True -> true | Neg b -> not (sat b) | And (b, c) -> sat b && sat c | Or (b,c) -> sat b || sat c | Imply (b,c) -> if sat b then sat c else true | Iff (b,c) -> if sat b then sat c else not (sat b) | Var b -> true

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> match e with | Const c->Const 0 | Var c -> if c = x then Const 1 else Var c | Power (c,d) ->  if c = x then Times[Const d; Power(c,d-1)] else Power(c,d)


(* problem 5*)
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp


let rec calculator : exp -> int
= fun e -> match e with INT b->b | ADD (c,d) -> calculator c + calculator d | SUB (c,d) -> 
calculator c - calculator d | MUL (c,d) -> calculator c * calculator d | DIV (c,d) -> calculator c / calculator d
