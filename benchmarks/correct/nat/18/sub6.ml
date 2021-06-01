type nat = ZERO | SUCC of nat

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
match (n1, n2) with
  |(ZERO, n2) -> n2
  |(n1, ZERO) -> n1
  |(SUCC(n), n2) -> natadd n (SUCC n2);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
match (n1, n2) with
  |(ZERO, n2) -> ZERO
  |(n1, ZERO) -> ZERO
  |(SUCC(n), n2) -> natadd (natmul n n2) n2;;

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
