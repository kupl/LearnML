(* Problem 1 *)
let rec pascal : int * int -> int = fun (x,y) -> 1;;

let rec pascal (x,y) =
	if (x<y) then raise (Failure "first_value is cannot bigger than second_value")
	else if (x=y) then 1
	else if (y=0) then 1
	else (pascal (x-1,y-1)) + (pascal (x-1,y));;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int = fun f a b -> 1;;

let rec sigma f a b =
  if (a=b) then f b
	else if (a>b) then (f a) + (sigma f (a-1) b)
	else (f a) + (sigma f (a+1) b);;

(* Problem 3 *)
let rec max : int list -> int = fun l -> 1;;
let rec max l =
	match l with
	[] -> 0
| [a] -> a
| h::t -> if (h > (max t)) then h else (max t);;

let rec min : int list -> int = fun l -> 1;;
let rec min l =
	match l with
	[] -> 0
| [a] -> a
| h::t -> if(h < (min t)) then h else (min t);;

(* Problem 4 *)
type formula =
True
| False
| Neg of formula
| Or of formula * formula
| And of formula * formula
| Imply of formula * formula
| Equiv of formula * formula;;

let eval : formula -> bool = fun f -> true;;

let eval f =
	match f with
	True -> true
| False -> false
| Neg a -> if a = True then false else true
| Or (a,b) ->
    if a = True then true
    else if b = True then true
    else false
| And (a,b) ->
		if a = False then false
    else if b = False then false
    else true
| Imply (a,b) ->
    if a = False then true
    else if  b = True then true
    else false
| Equiv (a,b) ->
    if a = True then (if b = True then true else false)
    else (if b = False then true else false)

(* Problem 5 *)
type nat = ZERO | SUCC of nat;;
let rec natadd : nat -> nat -> nat = fun n1 n2 -> ZERO;;

let rec natadd a b =
	match a with
	ZERO -> b
| (SUCC c) -> SUCC(natadd c b);;

let rec natmul : nat -> nat -> nat = fun n1 n2 -> ZERO;;

let rec natmul a b =
	match b with
	ZERO -> ZERO
| SUCC c -> (natadd a (natmul a c));;

