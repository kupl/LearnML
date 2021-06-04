type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat =
 fun n1 n2 ->
  let rec func n1 n2 =
    match n1 with ZERO -> n2 | SUCC n1 -> SUCC (func n1 n2)
  in
  func n1 n2


let natmul : nat -> nat -> nat =
 fun n1 n2 ->
  let funa n2 = match n2 with ZERO -> ZERO | SUCC n2 -> n2 in

  let funb n2 = match n2 with ZERO -> ZERO | SUCC n2 -> n2 in

  let k = funb n1 in

  let rec func n1 n2 =
    match n1 with
    | ZERO -> if funa n2 != ZERO then SUCC (func k (funb n2)) else ZERO
    | SUCC n1 -> if n2 != ZERO then SUCC (func n1 n2) else ZERO
  in
  func n1 n2


let natexp : nat -> nat -> nat =
 fun n1 n2 ->
  let funa n2 = match n2 with ZERO -> ZERO | SUCC n2 -> n2 in

  let sum = n1 in

  let mult n1 = natmul sum n1 in

  let rec funb n1 n2 =
    match n2 with
    | ZERO -> SUCC ZERO
    | SUCC n2 -> if funa n2 != ZERO then funb (mult n1) (funa n2) else mult n1
  in
  funb n1 n2
