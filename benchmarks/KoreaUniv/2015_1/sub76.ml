(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if y=0 || y=x then 1 else pascal (x-1,y-1) + pascal (x-1,y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
if a>b then raise (Failure "error")
else if a=b then f a
else f a + sigma f (a+1) b

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
|hd::tl->if hd > max tl then hd else max tl
|[] -> -999

let rec min : int list -> int
=fun l -> match l with
|hd::tl->if hd <  min tl then hd else min tl
|[] -> 999

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
=fun f -> 
match f with
|True -> true
|False-> false
|Neg f1 -> (match eval f1 with
		|true -> false
		|false -> true)
|Or (form1,form2) -> (eval form1) || (eval form2)
|And (form1,form2) -> (eval form1) && (eval form2)
|Imply (form1,form2) ->
	 (match eval form1 with
		|true -> eval form2
		|false -> true)
|Equiv (form1,form2) ->
	if eval form1=eval form2 then true else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
match n2 with
|ZERO -> n1
|SUCC n3 -> natadd (SUCC n1) n3

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
if n1 = ZERO then ZERO else
(match n2 with
|ZERO -> ZERO
|SUCC n3 -> natadd n1 (natmul n1 n3)
)
