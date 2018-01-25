(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> let rec pascal <a,b> = 
   if b=0 || b=a then 1
   else pascal <a-1,b-1> + pascal <a-1,b>;;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> let rec sigma f a b =
   if a=b then a
   else b + sigma f a b-1;;
   

(* Problem 3 *)
let rec max : int list -> int
=fun l -> let rec max lst : int =
   |hd::tl -> 
   if tl = [] then hd
   else if hd><max tl> then hd
   else max tl;;

let rec min : int list -> int
=fun l -> let rec min 1st : int =
   |hd::tl ->
   if tl = [] then hd
   else if hd<<min tl> then hd
   else min tl;;

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
=fun f -> true (* TODO *)

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO(* TODO *)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO (* TODO *)
