type nat = ZERO | SUCC of nat;;
let rec tonum word initial=
    if (initial = word) then 0
    else 1 + (tonum word (SUCC(initial)));;


let rec repeat n=
  match n with
    0-> ZERO
    |_-> SUCC( repeat (n-1));;



let natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  
  repeat ((tonum n1 ZERO) + (tonum n2 ZERO ));;


let natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  repeat ((tonum n1 ZERO) * (tonum n2 ZERO));;

let two =SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
natmul two three;;
natadd two three;;
(*
# let two = SUCC (SUCC ZERO);;
val two : nat = SUCC (SUCC ZERO)
# let three = SUCC (SUCC (SUCC ZERO));;
val three : nat = SUCC (SUCC (SUCC ZERO))
# natmul two three;;
- : nat = SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))
# natadd two three;;
- : nat = SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))
*)