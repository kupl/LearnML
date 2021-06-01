
(*2*)
type nat = ZERO | SUCC of nat;;
let rec natadd : nat ->nat ->nat = fun n1 n2 ->
  match n1 with
  ZERO -> n2
| SUCC (n3) -> natadd n3 (SUCC(n2)) ;;

let rec  natmul : nat->nat->nat = fun n1 n2 ->
  let n4 = n1 in  
  let rec multemp : nat->nat->nat->nat = fun n1 n2 n4 ->
  match n2 with
  ZERO -> ZERO
| SUCC ZERO -> n1
| SUCC (n5) -> multemp (natadd n4 n1) n5 n4 in multemp n1 n2 n4 ;;

let natexp : nat ->nat -> nat = fun n1 n2 ->
  let n4 = n1 in
  let rec exptemp : nat->nat->nat->nat = fun n1 n2 n4 ->
  match n2 with
  ZERO -> SUCC ZERO
| SUCC ZERO -> n1
| SUCC (n5) -> exptemp (natmul n4 n1) n5 n4 in exptemp n1 n2 n4;;
