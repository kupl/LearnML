type nat = ZERO | SUCC of nat;;
let rec num_nat (a,n) =
  match a with
  |ZERO -> n
  |SUCC k -> num_nat (k, (n+1));;
let rec f_SUCC (a, n) = 
  if n=0 then a
  else SUCC(f_SUCC(a, (n-1)));;
(*why f_SUCC a n or f_SUCC (n,a) don't work out?*)

let natadd : nat -> nat -> nat
= fun n1 n2 -> f_SUCC (ZERO, (num_nat(n1, 0))+(num_nat(n2 ,0)));;
let natmul : nat -> nat -> nat
= fun n1 n2 -> f_SUCC (ZERO, (num_nat(n1, 0))*(num_nat(n2 ,0)));;