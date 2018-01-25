(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
  if x == y || x <= 0 || y <= 0 then 
    1 
  else 
    pascal((x-1), (y-1)) + pascal(x,(y-1))
;;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
  if a == b then 
    f a 
  else 
    f a + sigma f (a+1) b
;;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 
  match l with
  | [] -> 0
  | hd :: [] -> hd
  | hd :: tl ->
      let r = max tl in 
      if hd > r then
        hd
      else
        r
;;

let rec min : int list -> int
=fun l -> 
  match l with
  | [] -> 0
  | hd :: [] -> hd
  | hd :: tl ->
      let r = min tl in
      if hd < r then
        hd
      else
        r
;;

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
  | Neg(f) -> if (eval f) then false else true
  | Or(f1, f2) -> if ((eval f1) || (eval f2)) then true else false
  | And(f1, f2) -> if ((eval f1) && (eval f2)) then true else false
  | Imply(f1, f2) -> if ((eval (Neg(f1))) || (eval f2)) then true else
    false
  | Equiv(f1, f2) -> if (eval f1) = (eval f2) then true else false
;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
  match n2 with
  | ZERO -> 
      let rec calcnat r = 
        match r with
        | ZERO -> ZERO
        | SUCC (r2) -> SUCC (calcnat r2)
      in
      calcnat n1
  | SUCC (n22) -> SUCC (natadd n1 n22)
;;

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
  match n2 with
  | ZERO -> ZERO
  | SUCC(n22) -> natadd n1 (natmul n1 n22)
;;
