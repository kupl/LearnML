let rec pascal : int * int -> int
=fun (x,y) ->
if x<y then 0
else if y=0 then 1
else if x=y then 1
else pascal ((x-1),(y-1))+pascal((x-1),(y));;


(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
if a > b then 1
else (f a ) * sigma f(a+1) b;;
(*
(* Problem 3 *)
let rec max : int list -> int
=fun l ->
 let bigger x y =
	if x>y then x else y
in let rec foldl op result lst =
	match lst with
	|[] -> result
	|hd::tl -> foldl op (op result hd) tl;;
in match l with
	|[] -> 0
	|hd::tl -> foldl bigger 0 l
*)

let rec min : int list -> int
=fun l -> 1 (* TODO *)

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
=fun f ->  (* TODO *)
match f with
|True -> true
|False -> false
|Neg(f')-> if f'=True then false else true
|Or(f',f'')-> if f'=False && f''=False then false else true
|And(f',f'')-> if f'=True && f''=True then true else false
|Imply(f',f'')-> if f'=True && f''=False then false else true
|Equiv(f',f'')->if f'=f'' then true else false
(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO(* TODO *)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO (* TODO *)
