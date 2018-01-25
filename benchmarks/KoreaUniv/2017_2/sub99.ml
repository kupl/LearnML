(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> match t with
  Node(x,l,r) -> Node(x,r,l)
  |Empty -> Empty;;


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
ZERO -> n1
| SUCC n2 -> SUCC (natadd n1 n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
ZERO -> ZERO
| SUCC n2 -> natadd n1 (natmul n1 n2)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
ZERO -> SUCC ZERO
| SUCC n2 -> natmul n1 (natexp n1 n2);;


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

let rec sat : formula -> bool
= fun f -> match f with
 True -> true
| False -> false
| Var x -> if true then true else false
| Neg e -> not (sat e)
| And (a,b) -> (sat a) && (sat b)
| Or (a,b) -> (sat a) || (sat b)
| Imply (a,b) -> not (sat a) || (sat b)
| Iff (a,b) -> (sat a) = (sat b);;

(*
(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> (* TODO *)
*)

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> match e with
  | X -> calculator X
  | INT i -> i
  | ADD (a,b) -> calculator a + calculator b
  | SUB (a,b) -> calculator a - calculator b
  | MUL (a,b) -> calculator a * calculator b
  | DIV (a,b) -> calculator a / calculator b
  | SIGMA (a,b,c) -> if calculator a > calculator b then 0    else calculator c;;


(*
(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> (* TODO *)


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)*)
