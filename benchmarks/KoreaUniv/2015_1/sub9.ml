(* Problem 1 *)
let rec pascal : int * int -> int
  = fun (x,y) -> 
    if y=0 then 1 else
    if x=y then 1 else pascal(x-1, y-1)+pascal(x-1, y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
  = fun f a b -> 
    if a>b then 0 else f a + sigma f (a+1) b;;

(* Problem 3 *)
let rec max : int list -> int
  = fun l -> match l with
      [e] -> e
    | h::t  -> let v = max t in if h>=v then h else v;;

let rec min : int list -> int
  = fun l -> match l with
      [e] -> e
    | h::t  -> let v = min t in if h<=v then h else v;;

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
  = fun f -> match f with
      True -> true
    | False -> false
    | Neg(a) -> not(eval(a))
    | Or(a, b) -> eval(a) || eval(b)
    | And(a, b) -> eval(a) && eval(b)
    | Imply(a, b) -> eval(a) || eval(Neg(b))
    | Equiv(a, b) -> eval(a) = eval(b);;


(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
  = fun n1 n2 -> match n1 with
      ZERO -> n2
    | SUCC(a) -> SUCC(natadd a n2);;

let rec natmul: nat -> nat -> nat
  = fun n1 n2 -> match n1 with
      ZERO -> ZERO
    | SUCC(a) -> natadd n2 (natmul a n2);;


