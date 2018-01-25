(* 2014210036 김낙현*)

(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1
let rec pascal (x,y) =
  match (x,y) with 
  |(m,n) -> if m=n then 1
	else if m=0 then 1 else if n=0 then 1 else pascal (m-1,n-1) + pascal (m-1,n)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 1
let rec sigma f a b =
	if a<b then (f a)+(sigma f (a+1) b) else f a

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 1 
let rec max l =
	match l with
	|[n]->n+0
	|h1::t1 -> match t1 with |h2::t2->if h2>h1 then max ([h2]@t2) else max ([h1]@t2)

let rec min : int list -> int
=fun l -> 1
let rec min l =
	match l with
	|[n]->n+0
	|h1::t1 -> match t1 with |h2::t2->if h2<h1 then min ([h2]@t2) else min ([h1]@t2)


(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> true 
let rec eval a =
	match a with
	|True->true
	|False->false
	|Neg (n)->not(eval n)
	|Or (q,w)->(eval q)||(eval w)
	|And (q,w)->(eval q)&&(eval w)
	|Imply (q,w)->(eval (Neg (q)))||(eval w)
	|Equiv (q,w)->if q=w then true else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO
let rec natadd q w =
	match q with
	|ZERO -> w
	|SUCC(a) -> natadd a (SUCC(w))

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO
let rec natmul q w =
	match q with
	|SUCC ZERO -> w
	|SUCC(e) -> natadd (natmul e w) w