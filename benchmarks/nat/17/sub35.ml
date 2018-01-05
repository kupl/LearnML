type nat = ZERO | SUCC of nat

let rec natadd ((nat1 : nat), (nat2 : nat)) : nat =
  match nat2 with
  | ZERO -> nat1
  | SUCC nat2_pre -> natadd(SUCC nat1, nat2_pre) ;;

let rec natmul ((nat1: nat), (nat2 : nat)) : nat  =
  match nat2 with
  | ZERO -> ZERO
  | SUCC nat2_pre -> natadd(nat1, natmul(nat1, nat2_pre)) ;;