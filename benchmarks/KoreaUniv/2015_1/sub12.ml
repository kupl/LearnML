(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) ->  
if x == y || y == 0 then 1
else pascal(x-1, y-1) + pascal(x-1, y);; 


(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
if a < b then f a + sigma f (a+1) b
else f b;; 



(* Problem 3 *)
let rec max : int list -> int
=fun l ->  
match l with
| [] -> 0
| hd::tl -> (match tl with
      |[] -> hd +0
      |hd1::tl1 -> if hd>hd1 then max(hd::tl1) else max(hd1::tl1)
    );; 

let rec min : int list -> int
=fun l -> 
match l with
| [] -> 0
| hd::tl -> (match tl with
      |[] -> hd +0
      |hd1::tl1 -> if hd<hd1 then min(hd::tl1) else min(hd1::tl1)
    );; 


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
  | True -> true
  | False -> false
  | Neg a -> not(eval a)
  | Or (a, b) -> (eval a) || (eval b) 
  | And(a, b) -> (eval a) && (eval b)
  | Imply (a, b) -> (eval (Neg a)) || (eval b)
  | Equiv (a, b) -> if (eval a)=(eval b) then true else false;;



(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
  match n1 with
    |ZERO -> n2
    |SUCC a -> natadd 1 (SUCC n2);;   
  

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
  match n1 with
  |ZERO -> ZERO
  |SUCC ZERO -> n2
  |SUCC a -> natadd n2 (natmul a n2) ;;