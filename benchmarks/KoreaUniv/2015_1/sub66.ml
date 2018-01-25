(* Problem 1 *)
let rec pascal (x,y)=
if x=y then 1
else if y=0 then 1
else pascal(x-1,y-1)+pascal(x-1,y);; 

(* Problem 2 *)
let rec sigma ff a b =
if a<b then ((ff b) + (sigma ff a (b-1)))
else ff b;;

(* Problem 3 *)
let rec max l=
match l with
[] ->0
|h::t->if(h>(max t)) then h else (max t);; 

let rec min l=
match l with
[]->0
|h::t->if(h<(min t)) then h else (min t);;

(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval formula =
match formula with
 True->true
|False->false
|Neg a->not(eval a)
|Or (a,b)->(eval a)||(eval b)
|And (a,b)->(eval a)&&(eval b)
|Imply (a,b)->not (eval a)||(eval b)
|Equiv (a,b)-> if eval a= eval b then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd n1 n2=
match n1 with
ZERO->n2
|SUCC nat ->natadd nat(SUCC n2);;

let rec natmul n1 n2=
match n1 with
ZERO->ZERO
|SUCC nat ->if nat=ZERO then n2 else natmul nat(natadd n2 n2);;
