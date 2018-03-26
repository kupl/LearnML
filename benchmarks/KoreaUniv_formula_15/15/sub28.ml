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
