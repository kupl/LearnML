type nat = ZERO | SUCC of nat;;

let rec oneadd a =
match a with
ZERO -> ZERO
| SUCC (a1) -> SUCC (oneadd a1);;

let rec natadd a b =
match b with
ZERO ->  oneadd a
| SUCC (b1) -> SUCC (natadd a b1);;

let rec natmul a b = 
match b with
ZERO -> ZERO
| SUCC (b1) -> natadd a (natmul a b1);;
