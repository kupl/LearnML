type nat = ZERO | SUCC of nat

let rec getval e =
  match e with
    ZERO -> 0
  | SUCC x -> getval(x)+1

let rec expnat n =
  if (n == 0) then ZERO
  else SUCC(expnat(n-1))

let natadd(a, b)=
  expnat(getval(a)+getval(b))

let natmul(a, b)=
  expnat(getval(a)*getval(b))
