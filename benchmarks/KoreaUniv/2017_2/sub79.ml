(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->

match t with
| Empty -> Empty
| Node(num, left, right) -> Node(num, mirror right, mirror left)


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->

match n1 with
| ZERO -> n2
| SUCC n1m1 -> SUCC (natadd n1m1 n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with
| ZERO -> ZERO
| SUCC n1m1 -> natadd n2 (natmul n1m1 n2)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
match n2 with
| ZERO -> SUCC ZERO
| SUCC n2m1 -> natmul n1 (natexp n1 n2m1)

(*
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

let sat : formula -> bool
= fun f -> (* TODO *)
*)

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

let rec xtoint : exp -> int -> int
= fun exp i ->
match exp with
| INT a -> a
| X -> i
| ADD (a,b) -> xtoint a i + xtoint b i
| SUB (a,b) -> xtoint a i - xtoint b i
| MUL (a,b) -> xtoint a i * xtoint b i
| DIV (a,b) -> xtoint a i / xtoint b i

let rec calculator : exp -> int
= fun e ->
match e with
| INT a -> a
| ADD (a,b) -> calculator a + calculator b
| SUB (a,b) -> calculator a - calculator b
| MUL (a,b) -> calculator a * calculator b
| DIV (a,b) -> calculator a / calculator b
| SIGMA (first, last, expr) -> 
if (calculator first) > (calculator last) then 0
else (xtoint expr (calculator first)) + (calculator (SIGMA((ADD(first,INT 1)), last, expr)))


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int


let rec bsum : branch -> int
= fun b ->
match b with
| SimpleBranch (a,b) -> b
| CompoundBranch (a,b) -> 
let (p,q) = b in bsum p + bsum q


let rec msum : mobile -> int
= fun m -> let (a,b)=m in (bsum a) + (bsum b) 


let rec balanced : mobile -> bool
= fun m ->
match m with
| (SimpleBranch (a,b), SimpleBranch (c,d)) -> if a*b = c*d then true else false
| (CompoundBranch(a,b), SimpleBranch (c,d)) -> 
if a*(msum b) = c*d then balanced b else false
| (SimpleBranch(a,b), CompoundBranch (c,d)) -> 
if a*b = c*(msum d) then balanced d else false
| CompoundBranch (a,b), CompoundBranch(c,d) -> 
if a*(msum b) = c*(msum d) then balanced b && balanced d else false



(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec twoto10 : bin -> int -> int
= fun l k ->
match l with
|[]->0
| hd::tl ->
if hd = ONE then (int_of_float (2.0**(float_of_int k))) + (twoto10 tl (k+1))
else twoto10 tl (k+1)

let rec tento2 i = 
if i=0 then []
else if i mod 2 = 1 then (tento2 (i/2))@[ONE]
else (tento2 (i/2))@[ZERO]

let rec bmul : bin -> bin -> bin
= fun b1 b2 ->
tento2 ((twoto10 (List.rev b1) 0) * (twoto10 (List.rev b2) 0))
