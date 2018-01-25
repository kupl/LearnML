(* Problem 1 *)
let rec pascal : int * int -> int
= fun (x, y) ->  
	if x = 0 then 1
	else if x = 1 then 1
	else if y = 0 then 1
	else if x = y then 1	
	else pascal(x -1, y-1) + pascal(x-1, y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int 
= fun f a b ->
	if a < b then f a + sigma f (a+1) b
	else f b

(* Problem 3 *)
let rec max : int list -> int
=fun l ->  
	match l with
	|[] -> 0
	|d::[] -> d
	|d::e -> 
		let f = max e in
		if d > f then d
		else f

let rec min l = 
	match l with
	|[] -> 0
	|d::[] -> d
	|d::e -> 
		let f = min e in
		if d < f then d
		else f
(* Problem 4 *)
type formula =
	|True
	|False
	|Neg of formula
	|Or of formula * formula	
	|And of formula * formula	
	|Imply of formula * formula
	|Equiv of formula * formula
let rec  eval : formula -> bool
= fun fm
	match fm with
	|True -> true
 	|False -> false
	|Neg a -> not(eval(a))
	|Or(a, b) -> eval(a) || eval(b)
	|And(a, b) -> eval(a) && eval(b)
	|Imply(a,b) -> not(eval(a)) || eval(b)
	|Equiv(a,b) -> (not(eval(a))||eval(b)) && (eval(a)||not(eval(b)))
	

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun x y->
	match x with
	|ZERO -> y
	|SUCC n -> natadd n(SUCC y)

let rec natmul : nat -> nat -> nat
=fun x y ->
	match x with
	|ZERO -> ZERO
	|SUCC ZERO ->y
	|SUCC n -> natadd y(natmul n y)