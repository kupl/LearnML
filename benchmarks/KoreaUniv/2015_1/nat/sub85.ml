type nat = ZERO | SUCC of nat;;
let rec natadd m : function
ZERO -> m
| SUCC n-> natadd (SUCC m) n;;

let rec natmul a b = match b with
ZERO -> ZERO
| SUCC n1 -> add (natmul a n) a;;