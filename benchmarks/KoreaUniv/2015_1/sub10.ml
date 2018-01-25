exception WrongInput

let pascal : int * int -> int
=fun (x,y) -> let rec factorial x=match x with
1 -> 1
|_ -> x * factorial(x-1)
in if y<0||x<0||y>x then raise WrongInput
else if y=0||x=y then 1 else factorial x/((factorial y)*factorial (x-y));;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a>b then raise WrongInput else match b-a with
0 -> f a
|_ -> sigma f (a+1) b + f a;;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
[] -> raise WrongInput
|[x]-> x
|h::t -> if h>max t then h else max t;;  

let rec min : int list -> int
=fun l -> match l with
[] -> raise WrongInput
|[x]-> x
|h::t -> if h<min t then h else min t;;

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
=fun f -> match f with 
Neg f1 -> if f1=True then false else true
| Or (f1,f2) -> if f1=False&&f2=False then false else true
| And (f1,f2) -> if f1=False||f2=False then false else true
| Imply (f1,f2) -> if f1=True&&f2=False then false else true
| Equiv (f1,f2) -> if f1=f2 then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n2 with
ZERO -> n1 |
SUCC n3-> natadd (SUCC n1) n3;;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> let rec nataddmul : nat -> nat -> nat -> nat
=fun n1 n2 n3-> match n3 with
SUCC(ZERO) -> n1 |
SUCC n3 -> nataddmul (natadd n1 n2) n2 n3 in
if n1=ZERO||n2=ZERO then ZERO else nataddmul n1 n1 n2;;