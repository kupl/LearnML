type nat =
	| ZERO
	| SUCC of nat

let rec chg_nat : nat->int
=fun n-> match n with
|ZERO -> 0
|SUCC ZERO -> 1
|SUCC(a) -> chg_nat a+1;;

let rec chg_int : int->nat
=fun n -> match n with
|0->ZERO
|1->SUCC ZERO
|a->SUCC (chg_int(a-1));;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> chg_int(chg_nat n1 + chg_nat n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> chg_int(chg_nat n1 * chg_nat n2);;
