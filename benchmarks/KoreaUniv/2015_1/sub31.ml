(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1
  if y=x then 1 

  else if y=0 then 1

  else pascal(x-1,y-1)+pascal(x-1,y)


(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 1
  if b=a then (f a)
  else (f b)+sigma f a (b-1)

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 1 
match l with 
  |[]->0
  |h::t-> if h>(max t) then h 
  else (max t)

let rec mi1n : int list -> int
=fun l -> 1
 match l with 
  |[]->0
  |h::t-> if h<(min t) then h 
  else (min t)

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
match f with 
	|True->true
	|False->false
	|Neg a-> not(eval a)
	|Or (x,y)->(eval x)||(eval y)
	|And (c,d)->(eval c)&&(eval d)
	|Imply (i,j)-> if (eval i)==false || (eval j)==true then true 
	 else false
|Equiv (m,n)-> if (eval m)==(eval n) then true else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO 
match n1 with 

	|ZERO -> n2

	|SUCC(a)-> natadd a (SUCC(n2))

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO
if n2=ZERO then ZERO

  else if n2=SUCC(ZERO) then n1

  else 

   match n1 with 

   |ZERO -> ZERO

   |SUCC(ZERO)-> n2 

   |SUCC(a) -> natadd (natmul a n2) n2
