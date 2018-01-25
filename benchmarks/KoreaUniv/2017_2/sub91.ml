(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> 
match t with
|Empty -> Empty
|Node (i, n1, n2) -> Node (i, mirror n2, mirror n1)

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with
| ZERO -> n2
| SUCC n1 -> SUCC(natadd n1 n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
match n1 with
| ZERO -> ZERO
| SUCC n1 -> natadd n2 (natmul n1 n2)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> 
match n1 with
| ZERO -> raise (Failure ("Invalid_argument"))
| SUCC ZERO -> SUCC ZERO
| _ ->
        match n2 with
        | ZERO -> SUCC ZERO
        | SUCC ZERO -> n1
        | SUCC n2 -> natmul n1 (natexp n1 n2)

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

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) ->
match e with
|Const n -> Const 0
|Var v -> if v=x then Const 1 else Const 0
|Power (s, i) ->
if s=x then Times[Const i; Power(s, i-1)] else Const 0
|Times [] -> Const 0
|Times (hd::tl) -> Sum [Times [diff (hd,x); Times tl]; Times [diff (Times tl,x); hd]]
|Sum [] -> Const 0
|Sum (hd::tl) -> Sum [diff (hd, x); diff (Sum tl, x)]

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e ->
match e with
| INT n -> n
| ADD (e1, e2) -> (calculator e1)+(calculator e2)
| SUB (e1, e2) -> (calculator e1)-(calculator e2)
| MUL (e1, e2) -> (calculator e1)*(calculator e2)
| DIV (e1, e2) -> (calculator e1)/(calculator e2)
 


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec scale
= fun bm ->
match bm with
|SimpleBranch(l,w)-> l*w 
|CompoundBranch(l,m)-> match m with
|(left,right) -> scale left + scale right

let balanced : mobile -> bool
= fun m ->
 match m with
|(l,r)-> match l, r with
|(CompoundBranch(ln,mob),SimpleBranch(len,we))->if (ln* scale l)-(scale r) >0 ||(scale r) - (ln * scale l) >0 then false else true
|(SimpleBranch(le,we),CompoundBranch(lng,mo))->if (scale l)-(lng*scale r) >0 ||(lng*scale r) - (scale l) >0 then false else true
|(CompoundBranch(ln,mob),CompoundBranch(lng,mo))->if (ln* scale l)-(lng*scale r) >0 ||(lng*scale r) - (ln * scale l) >0 then false else true
|(SimpleBranch(le,we),SimpleBranch(len,w))->if (scale l)-(scale r) >0 ||(scale r) - (scale l) >0 then false else true

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec digittoint l =
match l with
|[]->[]
|hd::tl -> if hd = ONE then digittoint tl@[1] else digittoint tl@[0]

let rec inttodigit l =
match l with
|[]->[]
|hd::tl -> if hd = 1 then inttodigit tl@[ONE] else inttodigit tl@[ZERO]

let rec bintoint l =
match l with
|[]->0
|hd::tl -> hd+(2*bintoint tl)

let rec inttobin n =
match n with
|0 -> []
|n -> [n mod 2]@inttobin(n/2)

let rec bmul : bin -> bin -> bin
= fun b1 b2 ->
let a = bintoint(digittoint b1) in
let b = bintoint(digittoint b2) in
let c = (inttobin (a*b)) in
if inttodigit c=[] then [ZERO] else inttodigit c

