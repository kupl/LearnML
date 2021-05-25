type nat = ZERO | SUCC of nat;;

let rec evalnat a = 
match a with
| ZERO -> 0
| SUCC n -> evalnat n + 1;;

let rec evalint a =
match a with
| 0 -> ZERO
| 1 -> SUCC ZERO
| n -> SUCC (evalint (n-1));;

let natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
evalint(evalnat n1 + evalnat n2);;

let natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
evalint(evalnat n1 * evalnat n2);;

let two = (SUCC (SUCC ZERO));;
let three = SUCC (SUCC (SUCC ZERO));;
natmul two three;;
natadd two three;;

(* Original Code
type nat = ZERO | SUCC of nat

let rec evalnat a = 
match a with
| ZERO -> 0
| SUCC n -> evalnat n + 1

let rec evalint a =
match a with
| 0 -> ZERO
| 1 -> SUCC ZERO
| n -> SUCC (evalint (n-1))

let natadd a b = evalint(evalnat a + evalnat b)

let natmul a b = evalint(evalnat a * evalnat b)
*)