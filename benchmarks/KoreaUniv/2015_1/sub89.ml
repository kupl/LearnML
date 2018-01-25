(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1 (* TODO *)let rec pascal (x,y)=
	if x<y then raise(Failure "error") else if y=0 then 1 else if x=y then 1 else pascal (x-1,y-1)+pascal (x-1,y);;
(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 1 (* TODO *)

let rec sigma f a b=
if b=a then (f a)   else (f b) + (sigma f a (b-1));;
(* Problem 3 *)
let rec max : int list -> int
=fun l -> 1 (* TODO *)

let rec min : int list -> int
=fun l -> 1 (* TODO *)let rec max l = 
	match l with 
		| [a]->a
		| hd::tl-> if hd > (max tl) then hd else (max tl);;
let rec min : int list -> int
=fun l -> 1;;
let rec min l =
	match l with
		| [a]->a
		|hd::tl-> if hd <(min tl) then hd else (min tl);;

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
=fun f -> true (* TODO *)type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula;;

let rec eval : formula -> bool
=fun f -> true ;;

let rec eval f=
	match f with
	|True->true
	|False->false
	|Neg(f1)->if (eval f1) = true then false else true
	|Or(f1,f2)->if ((eval f1)&&(eval f2))=false then false else true
	|And(f1,f2)->if ((eval f1)&&(eval f2))=true then true else false
	|Imply(f1,f2)->if ((eval f1)&&(eval f2))=true then true else if (eval f1)=false then true else false
	|Equiv(f1,f2)->if(eval f1)=(eval f2) then true else false;;


(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO(* TODO *)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO (* TODO *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO


let rec natadd n1 n2=
	match n2 with
		|ZERO->n1
		|SUCC(n)->SUCC(natadd n1 n);;	


let rec natmul n1 n2=
	match n2 with
		|ZERO->ZERO
		|SUCC(ZERO)->n1
		|SUCC(n)->natadd n1 (natmul n1 n);;



