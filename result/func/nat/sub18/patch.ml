type nat = ZERO | SUCC of nat

let __s1 (__s2 : nat) : bool = if __s2 = ZERO then true else false

let natadd (n1 : nat) (n2 : nat) : nat =
  let rec func (n1 : nat) (n2 : nat) : nat =
    match n1 with ZERO -> n2 | SUCC n1 -> SUCC (func n1 n2)
  in
  func n1 n2


let natmul (n1 : nat) (n2 : nat) : nat =
  let funa (n2 : nat) : nat = match n2 with ZERO -> ZERO | SUCC n2 -> n2 in

  let funb (n2 : nat) : nat = match n2 with ZERO -> ZERO | SUCC n2 -> n2 in

  let k : nat = funb n1 in

  let rec func (n1 : nat) (n2 : nat) : nat =
    if __s1 n1 then ZERO
    else if __s1 n1 then ZERO
    else match n1 with SUCC __s9 -> natadd n2 (func n2 __s9) | ZERO -> ZERO
  in
  func n1 n2


let natexp (n1 : nat) (n2 : nat) : nat =
  let funa (n2 : nat) : nat = match n2 with ZERO -> ZERO | SUCC n2 -> n2 in

  let sum : nat = n1 in

  let mult (n1 : nat) : nat = natmul sum n1 in

  let rec funb (n1 : nat) (n2 : nat) : nat =
    match n2 with
    | ZERO -> SUCC ZERO
    | SUCC n2 -> if funa n2 != ZERO then funb (mult n1) (funa n2) else mult n1
  in
  funb n1 n2
