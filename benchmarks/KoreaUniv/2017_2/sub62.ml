(*problem 1*)

type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
match t with
|Empty -> Empty
|Node(n, a, b) -> Node(n, mirror b, mirror a);;

(*problem 2*)

type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
match n2 with
ZERO -> n1
|SUCC( k) -> natadd (SUCC n1) k;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
let n3 = n1 in
let rec hold1 = fun n1 n2 n3 ->
match n2 with
ZERO -> ZERO
|SUCC ZERO -> n1
|SUCC (k) -> hold1 (natadd n1 n3) k n3 in hold1 n1 n2 n3;;

let rec natexp : nat -> nat -> nat
= fun n1 n2 ->
let n3 = n1 in
let rec hold2 = fun n1 n2 n3->
match n2 with
ZERO -> SUCC ZERO
|SUCC ZERO -> n1
|SUCC (k) -> hold2 (natmul n1 n3) k n3 in hold2 n1 n2 n3;;

(*problem 4*)
(*
type aexp =
|Const of int
|Var of string
|Power of string * int
|Times of aexp list
|Sum of aexp list

diff : aexp * string -> aexp
= fun n ->
*)

(*problem 6*)

type mobile = branch * branch
and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
and length = int
and weight = int

let rec weightsum : mobile -> int
= fun n1-> 
match n1 with
SimpleBranch(a, b) , CompoundBranch(c, d) -> (b + weightsum d)
|CompoundBranch(a, b) , CompoundBranch(c, d) -> (weightsum b + weightsum d)
|CompoundBranch(a, b) , SimpleBranch(c, d) -> (weightsum b + d)
|SimpleBranch(a, b) , SimpleBranch(c, d) -> (b + d);;

let rec balanced : mobile -> bool
= fun c ->
match c with
SimpleBranch(a,b) , SimpleBranch(c,d) -> 
if a*b = c*d then true
else false
|CompoundBranch(a,b) , SimpleBranch(c,d) ->
if balanced b = true then
if a * weightsum b = c * d then true
else false
else false
|SimpleBranch(a,b) , CompoundBranch(c,d) ->
if balanced d = true then
if a*b = c*weightsum d then true
else false
else false
|CompoundBranch(a,b) , CompoundBranch(c,d) ->
if balanced b = true && balanced d = true then
if a*weightsum b = c*weightsum d then true
else false
else false;;
