type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> 
  let rec num n=
    match n with
      ZERO-> 0
      |SUCC(x) -> 1 + num(x) in
  let ans = (num n1)+ (num n2) in
  let rec conclusion m = 
    match m with
      0-> ZERO
      |1-> SUCC ZERO
      |_-> let f = conclusion(m-1) in SUCC(f) in (conclusion ans);;

let natmul : nat -> nat -> nat
= fun n1 n2 -> 
  let rec num n = 
    match n with
      ZERO-> 0
      |SUCC(x) -> 1+ num(x) in
  let ans = (num n1)*(num n2) in
  let rec conclusion m =
    match m with
      0->ZERO
      |1-> SUCC ZERO
      |_-> let f = conclusion(m-1) in SUCC(f) in (conclusion ans);;
      
      
  
  
