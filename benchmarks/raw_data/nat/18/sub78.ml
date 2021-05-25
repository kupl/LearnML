type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> let rec nat_add_count = fun a1 a2-> if a1<>n1 then nat_add_count (SUCC a1) (SUCC a2) else a2 in
                nat_add_count ZERO n2;;

let natmul : nat -> nat -> nat
= fun n1 n2 -> let rec nat_mul_count = fun b1 b2 -> if b1<>n1 then nat_mul_count (SUCC b1) (natadd n2 b2) else b2 in
                        nat_mul_count ZERO ZERO;;



let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;


natmul two three;;
natadd two three;;
