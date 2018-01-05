type nat = ZERO
           | SUCC of nat

let rec nat2int x =
  match x with
    ZERO -> 0
    | SUCC (y) -> nat2int y + 1

let rec int2nat x =
  if x == 0 then ZERO
  else SUCC (int2nat (x-1))

let rec natadd (nat1, nat2) =
  int2nat (nat2int nat1 + nat2int nat2);;

let rec natmul (nat1, nat2) =
  int2nat (nat2int nat1 * nat2int nat2);;
