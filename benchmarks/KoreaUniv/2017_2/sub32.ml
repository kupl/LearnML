(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun tree -> 
match tree with
| Empty -> tree
| Node(a, t1, t2) ->
begin if t1 = Empty then Node(a, mirror t2, Empty)
else if t2 = Empty then Node(a, Empty, mirror t1)
else Node(a, mirror t2, mirror t1)
end;;


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with
|ZERO -> n2
|SUCC r -> natadd r (SUCC n2);;

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with
|ZERO -> ZERO
|SUCC r -> natadd (natmul r n2) n2;;

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
match n2 with
|ZERO -> SUCC(ZERO)
|SUCC r -> natmul n1 (natexp n1 r);;


(* problem 3*)
(* type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool
= fun f -> (*TODO*) *)


(* problem 4*)
module Problem4 = struct
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, var) -> 
match exp with
| Consti -> Const 0
| Var i -> if i = var then Const 1 else Const 0
| Power(i, n) -> if i = var then Times[Const n; Power(i, n-1)]
else Const 0;
| Times [] -> Const 0
| Times (m::n) -> Sum[Times(diff(m, var)::n); Times[m; diff(Times n, var)]]
| Sum [] -> Const 0
| Sum (m::[]) -> diff(m, var)
| Sum (m::n) -> Sum[diff(m, var); diff(Sum n, var)]
end;;


(* problem 5*)
module Problem5 = struct
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp ->
match exp with
| INT num -> num
| ADD(m, n) -> (calculator m) + (calculator n)
| SUB(m, n) -> (calculator m) - (calculator n)
| MUL(m, n) -> (calculator m) * (calculator n)
| DIV(m, n) -> (calculator m) / (calculator n)
| SIGMA(m, n, p) ->
if (calculator m) > (calculator n) then 0
else if (calculator m) = (calculator n) then 
begin
	let rec cal : exp*exp -> int
	= fun(ex, i) ->
	match ex with
	| X -> cal(i, i)
	| INT num -> num
	| ADD(a, b) -> cal(a, i) + cal(b, i)
	| SUB(a, b) -> cal(a, i) - cal(b, i)
	| MUL(a, b) -> cal(a, i) * cal(b, i)
	| DIV(a, b) -> cal(a, i) / cal(b, i)
	| SIGMA(a, b, c) -> calculator c
	in cal(p, m)
	end
	else calculator(SIGMA(ADD(m, INT 1), n, p)) + calculator(SIGMA(m, m, p))
	end;;


(* problem 6*)
module Problem6 = struct
type mobile = branch * branch     
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec plus : branch -> int
= fun p ->
match p with
| SimpleBranch(m, n) -> n
| CompoundBranch(m, (n, o)) -> (plus n) + (plus o);;

let rec mu1 : branch -> int
= fun mo ->
match mo with
| SimpleBranch(a, b) -> a * (plus mo)
| CompoundBranch(a, (b, c)) -> a * (plus mo);;

let rec balanced : mobile -> bool
= fun mob ->
let rec ba1 : branch -> bool
= fun br ->
match br with
| SimpleBranch(1, w) -> true
| CompoundBranch(1, (w1, w2)) -> if (mu1 w1) = (mu1 w2) then (ba1 w1) && (ba1 w2)
else false
in
match mob with
| (a, b) -> ((ba1 a) && (ba1 b)) && ((mu1 a) = (mu1 b))
end;;


(* problem 7*)
(* type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *) *)