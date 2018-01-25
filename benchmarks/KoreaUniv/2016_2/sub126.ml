let rec max l = match l with
	|[] -> 0
	|h::t -> if h > max t then h else max t;;

let rec min l = match l with
	|[] -> 0
	|h::t -> if h < min t then h else min t;;

let rec filter p l = match l with
	|[] -> []
	|h::t -> if p h then h::filter p t else filter p t;;

let inc x = x + 1;;
let mul x = x * 2;;
let rec double eq n = eq(eq n);;

type btree = Empty | Node of int * btree * btree;;
let rec mem n t = match t with
	|Empty -> false
	|Node (p,l,r) ->
	if p==n then true
	else if mem n l then true
	else if mem n r then true
	else false;;

type nat = ZERO | SUCC of nat;;
let rec natadd a b = match a with
	|ZERO -> b
	|SUCC(c) -> natadd c (SUCC b);;
let rec natmul a b = match a with
	|ZERO -> ZERO
	|SUCC(c) -> natadd (natmul c b) b;;

type formula =
	|True
	|False
	|Not of formula
	|AndAlso of formula * formula
	|OrElse of formula * formula
	|Imply of formula * formula
	|Equal of exp * exp
and exp =
	|Num of int
	|Plus of exp * exp
	|Minus of exp * exp;;

let rec exp_eval e = match e with
	|Num a -> a
	|Plus (a, b) -> exp_eval a + exp_eval b
	|Minus (a, b) -> exp_eval a - exp_eval b;;

let rec eval s = match s with
	|True -> true
	|False -> false
	|Not f -> if eval f then false else true
	|AndAlso (a, b) -> if eval a && eval b then true  else false
	|OrElse (a, b) -> if eval a || eval b then true else false
	|Imply (a, b) -> if eval a && eval b == false then false else true
	|Equal (a,b) -> if exp_eval a == exp_eval b then true else false;;
       