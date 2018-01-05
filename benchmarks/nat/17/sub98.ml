type nat = ZERO
          | SUCC of nat

let rec natadd ((num1 : nat), (num2: nat)) : nat =
  match num1 with
  | ZERO -> num2
  | SUCC(prec) -> natadd(prec, SUCC(num2))

let rec natmul ((num1 : nat), (num2: nat)) : nat =
  match num1 with
  | ZERO -> ZERO
  | SUCC(prec) -> natadd(num2, natmul(prec, num2))

(* let rec nat2int (num : nat) : int =
  match num with
  | ZERO -> 0
  | SUCC(prec) -> 1 + nat2int((prec))
*)