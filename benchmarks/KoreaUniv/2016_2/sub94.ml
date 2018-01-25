(*********************)
(*     Problem 1     *)
(*********************)
let rec max l =
match l with
| []-> 0
| hd::tl ->
if tl=[] then hd
else if hd< max tl then max tl
else hd;;


let rec min l =
match l with
| [] -> 0
| hd::tl ->
if tl=[] then hd
else if hd< min tl then hd
else min tl;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
match lst with
| [] -> []
| hd::tl ->
if pred hd = true then hd::filter pred tl
else filter pred tl;;


(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =
f(f a) ;;
(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem n btree=
match btree with
| Empty -> false
| Node(element,left_btree,right_btree)->
if  element==n then true
else mem n left_btree || mem n right_btree ;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd n1 n2 =
match n2 with
| ZERO ->  n1
| SUCC(nat)->
 SUCC(natadd n1 nat);;


let rec natmul n1 n2=
match n2 with
| ZERO -> ZERO
| SUCC(nat)->
natadd n1 (natmul n1 nat);;

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
	| Minus of exp * exp

let rec eval formula=
match formula with
| True-> true
| False-> false
| Not(x)->
if eval x=false then true
else false
| AndAlso(x,y) ->
if eval x=true&&eval y=true then true
else if eval x=true&&eval y=false then false
else if eval x=false&&eval y=true then false
else false
| OrElse(x,y)->
if eval x=true&&eval y=true then true
else if eval x=true&&eval y=false then true
else if eval x=false&&eval y=true then true
else false
| Imply(x,y)->
if eval x=true&&eval y=true then true
else if eval x=true&&eval y=false then false
else if eval x=false&&eval y=true then true
else true
| Equal(x,y)->
let rec exp fxy=
match fxy with
| Num n->n
| Plus(x,y)->exp x+exp y
| Minus(x,y)->exp x-exp y
in
if exp x==exp y then true
else false
;;
