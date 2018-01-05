type nat
  = ZERO
  | SUCC of nat

let rec natadd_helper (count_goal, current_nat, counter) =
  if count_goal = counter
  then current_nat
  else natadd_helper (count_goal, SUCC current_nat, SUCC counter)

let rec natadd (n1, n2) =
  match n1, n2 with
  | ZERO, n2 -> n2
  | n1, ZERO -> n1
  | n1, n2 -> natadd_helper (n1, n2, ZERO)

let rec natmul_helper (count_goal, multiple_by, current_nat, counter) =
  if count_goal = counter
  then current_nat
  else natmul_helper (count_goal, multiple_by, natadd (current_nat, multiple_by), SUCC counter)

let natmul (n1, n2) =
  match n1, n2 with
  | ZERO, n2 -> ZERO
  | n1, ZERO -> ZERO
  | n1, n2 -> natmul_helper (n1, n2, ZERO, ZERO)
