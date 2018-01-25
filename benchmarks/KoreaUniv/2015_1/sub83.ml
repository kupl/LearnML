(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1 ;;
let rec pascal (x,y) = if( x==0 || y==0 || x==y) then 1 else pascal (x-1,y) + pascal (x-1,y-1);;


(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 1 ;;

let rec sigma (f:int->int) (a:int) (b:int) = if a=b then (f a) else (f a) +(sigma f (a+1) b);;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 1;;
 
let rec max l= match l with | []->0 | hd::tl -> if  hd < (max tl) then (max tl) else hd ;;

let rec min : int list -> int
=fun l -> 1 ;;

let rec min l= match l with | hd::tl -> if tl==[] then hd else if hd > (min tl) then (min tl) else hd | [hd] -> hd ;;

(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula;;

let rec eval : formula -> bool
=fun f -> true;;

let rec eval f = match f with 
  |True -> true
  |False -> false
  |Neg a -> if (a == True) then false else true  
  |Or (a,b) ->( (eval a) || (eval b) )
  |And (a,b) -> ((eval a) && (eval b))
  |Imply (a,b) -> if (( (eval a)==true) && ((eval b)==false) ) then false else true
  |Equiv (a,b) ->  ((eval a)==(eval b));;

(* Problem 5 *)
type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO;;
let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO;;
let rec findnat x= if (x==0) then ZERO else SUCC(findnat (x-1));;
let rec natvalue n= match n with |ZERO -> 0 | SUCC(t) -> 1 +  (natvalue t);;
let rec down x= if (x==ZERO) then ZERO else findnat ((natvalue x)-1);; 
let rec natadd n1 n2 = if ((natvalue n1)==0) then n2 else (natadd (down n1) (SUCC(n2)) );;
let rec natmul n1 n2= if ( n2==ZERO ) then ZERO else (natadd n1 (natmul (down(n2)) n1) );; 



