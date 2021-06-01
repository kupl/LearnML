type nat = ZERO | SUCC of nat

let rec leng n = 
  match n with
    | ZERO -> 0
    | SUCC(n') -> 1 + leng n';;
    
let rec make_nat n = if (n = 0) then ZERO
                else SUCC(make_nat (n-1));;
                
let natadd : nat -> nat -> nat
= fun n1 n2 -> make_nat (leng n1 + leng n2);;(*TODO*)

let natmul : nat -> nat -> nat
= fun n1 n2 -> make_nat (leng n1 * leng n2);;(*TODO*)
