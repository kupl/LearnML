type nat = ZERO | SUCC of nat

let rec get_length : nat -> int 
= fun l ->
  match l with
  |ZERO -> 0
  |SUCC (l') -> 1 + get_length l';;
  
let rec make_nat : int -> nat 
=fun n ->
  match n with
  |0 -> ZERO
  |_ -> SUCC (make_nat (n-1));;
  
let natadd : nat -> nat -> nat
= fun n1 n2 -> 
  let a = get_length n1 and b = get_length n2 in
    let c = a+b in
      make_nat c;;
      
let natmul : nat -> nat -> nat
= fun n1 n2 -> 
  let a = get_length n1 and b = get_length n2 in
    let c = a*b in
      make_nat c;;
