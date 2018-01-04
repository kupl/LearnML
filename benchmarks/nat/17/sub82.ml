(* Mechanical & Aerospace Eng./2013-11706/Kang Injae/1-4.ml *)

type nat = ZERO | SUCC of nat

(*
let rec n2i (n : nat) : int =
  match n with
  | ZERO -> 0
  | SUCC m -> 1 + (n2i m)

let rec i2n (i : int) : nat =
  if i = 0 then ZERO
  else SUCC (i2n (i-1))
*)

let rec natadd ((a : nat), (b : nat)) : nat =
  match (a, b) with
  | (ZERO, _) -> b
  | (_, ZERO) -> a
  | (SUCC m, SUCC n) -> natadd (m, SUCC (SUCC n))

(*
let x = 3
let y = 7
let z = 12

let yx = n2i (natadd ((i2n y), (i2n x)))
let xy = n2i (natadd ((i2n x), (i2n y)))
let zx = n2i (natadd ((i2n z), (i2n x)))

let _ = (print_int yx); print_newline()
let _ = (print_int xy); print_newline()
let _ = (print_int zx); print_newline()
*)

let rec natmul ((a : nat), (b : nat)) : nat =
  match (a, b) with
  | (ZERO, _) -> ZERO
  | (_, ZERO) -> ZERO
  | (SUCC m, SUCC n) ->
      natadd (SUCC m, natmul(SUCC m, n))

(*
let yx = n2i (natmul ((i2n y), (i2n x)))
let _ = (print_int yx); print_newline()
*)
