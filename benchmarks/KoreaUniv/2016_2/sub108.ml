(*********************)
(*     Problem 1     *)
(*********************)

let rec max l = 
match l with 
|[] -> 0
|hd::tl-> if(hd>=max(tl)) then hd
else max(tl)

let rec min l =
match l with
|[] -> 10000
|hd::tl-> if(hd<=min(tl)) then hd
else min(tl)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
match lst with
|[] -> []
|hd::tl -> if (pred hd)
then hd :: filter pred tl
else filter pred tl


(*********************)
(*     Problem 3     *)
(*********************)

let rec double f a = f (f a);;


(*********************)
(*     Problem 4     *)
(*********************)
type btree = 
	| Empty
	| Node of int * btree * btree

let rec mem n tree =
match tree with 
| Empty -> false
| Node (a, tree1, tree2) -> if (a = n || mem n tree1 || mem n tree2) then true
else false


(*********************)
(*     Problem 5     *)
(*********************)

type nat = 
	|ZERO
	|SUCC of nat


let rec natadd n1 n2 =
match n1 with
|ZERO -> n2
|SUCC a -> SUCC (natadd a n2)


let rec natmul n1 n2 =
match n2 with
|ZERO -> ZERO
|SUCC a -> natadd n1 (natmul n1 a)


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
	|Num of int
	|Plus of exp * exp
	|Minus of exp * exp


let rec eval f =
match f with
| True -> true 
| False -> false
| Not a -> if eval a then false else true
| AndAlso (left, right) -> if eval left && eval right then true else false
| OrElse (left, right) -> if eval left||eval right then true else false
| Imply (left, right) -> if eval left && eval right then true else if eval left=true && eval right=false then true else false
| Equal (left, right) ->
	let rec env v = match v with
	|Num (a) -> a
	|Plus (a, b) -> env (a) + env (b)
	|Minus (a, b) -> env (a) - env (b)
in if (env (left) = env (right)) then true else false   
	 
