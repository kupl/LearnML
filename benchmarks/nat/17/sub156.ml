type nat =
  | ZERO
  | SUCC of nat;;

let rec natadd ((m: nat), (n: nat)): nat =
  match m with
  | ZERO    -> n
  | SUCC(x) -> SUCC(natadd (x, n));;

let rec natmul ((m: nat), (n: nat)): nat =
  match m with
  | ZERO    -> ZERO
  | SUCC(x) -> natadd (n, natmul (x, n));;
