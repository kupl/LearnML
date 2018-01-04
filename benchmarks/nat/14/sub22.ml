type nat = ZERO | SUCC of nat

let rec natadd eq =
  match eq with
  | (nat_x, ZERO) -> nat_x
  | (nat_x, (SUCC nat_y)) ->
     natadd ((SUCC nat_x), nat_y) ;;

let rec natmul eq =
  match eq with
  | (nat_x, ZERO) -> ZERO
  | (nat_x, (SUCC nat_y)) ->
     natadd (nat_x, natmul (nat_x, nat_y)) ;;
