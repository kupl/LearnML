(* Programmed by Dong Hyun Koo - 2009210036 *)
(* E-mail : tellmewhy07@gmail.com *)
(* Github : https://github.com/AlwaysAwake *)

(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if y = 0 || x = y then 1 else pascal(x-1, y-1) + pascal(x-1, y) (* TODO *)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
  if a = b then f b else f a + sigma f (a+1) b

(* Problem 3 *)
let rec max : int list -> int
=fun l ->
  match l with
  | [] -> 0
  | last :: [] -> last
  | hd :: tl ->
    let m = max(tl) in if hd >= m then hd else m 

let rec min : int list -> int
=fun l ->
  match l with
  | [] -> 0
  | last :: [] -> last
  | hd :: tl ->
    let m = min(tl) in if hd < m then hd else m

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
  | Neg(a) -> if a=True then false else true 
  | Or(a, b) -> if a=True || b=True then true else false
  | And(a, b) -> if a=True && b=True then true else false
  | Imply(a, b) -> if a=True && b=False then false else true
  | Equiv(a, b) -> if a=b then true else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
  let rec size n =
    match n with
    | ZERO -> 0
    | SUCC(a) -> 1 + size(a)
  in let rec make n = 
    match n with
    | 0 -> ZERO
    | _ -> SUCC(make (n-1))
  in make (size n1 + size n2)
let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
  let rec size n =
    match n with
    | ZERO -> 0
    | SUCC(a) -> 1 + size(a)
  in let rec make n = 
    match n with
    | 0 -> ZERO
    | _ -> SUCC(make (n-1))
  in make (size n1 * size n2)

