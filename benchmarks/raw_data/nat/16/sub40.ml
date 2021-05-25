type nat = ZERO | SUCC of nat

let rec natadd : nat->nat->nat = fun n1 n2 -> match n1 with |
ZERO -> n2 | SUCC(n3) -> SUCC(natadd n3 n2);;

let rec natmul : nat->nat->nat = fun n1 n2 ->
let rec mul a b c = match a with | ZERO -> c |
SUCC(a1) -> mul a1 b (natadd b c)in
mul n1 n2 ZERO;;
