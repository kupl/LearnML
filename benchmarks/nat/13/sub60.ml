type nat = ZERO | SUCC of nat

let rec nat2num n =
  match n with
  | ZERO -> 0
  | SUCC n2 -> 1 + (nat2num n2)

let rec num2nat n =
  if n=0 then ZERO
  else SUCC (num2nat (n-1))

let natadd (n1, n2) = num2nat ((nat2num n1) + (nat2num n2))

let natmul (n1, n2) = num2nat ((nat2num n1) * (nat2num n2))
