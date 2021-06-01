type nat = ZERO | SUCC of nat

let tointeger : nat -> int = fun na -> let rec findint : nat -> nat -> int -> int = fun org c ans -> if org=c then ans else findint org (SUCC(c)) (ans+1) in findint na ZERO 0;;
let tonat : int -> nat = fun na -> let rec findnat : int -> int -> nat -> nat = fun org c ans -> if org=c then ans else findnat org (c+1) (SUCC(ans)) in findnat na 0 ZERO;;

let natadd : nat -> nat -> nat
= fun n1 n2 -> tonat (tointeger n1 + tointeger n2);;

let natmul : nat -> nat -> nat
= fun n1 n2 -> tonat (tointeger n1 * tointeger n2);;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
natmul two three;;
natadd two three;;